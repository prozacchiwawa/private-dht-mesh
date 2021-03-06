module BCastRunner

open Util
open Buffer
open Bacon
open Broadcast
open DHTData
open BroadcastData

type InternalMsg<'ws> =
  | NoOp
  | SetId of string
  | NewSocket of (string * 'ws)
  | SocketClosed of string
  | WSReceive of (string * Buffer)
  | AddNode of string
  | RemoveNode of string
  | RpcRequestIn of (string * Serialize.Json)
  | RpcSuccess of (string * Serialize.Json)
  | RpcFailure of string
  | Tick
               
type Published =
  { subj : string
  ; reply : string option
  ; msg : string
  }

type Effect =
  | RpcRequest of (string * Serialize.Json)
  | WSSend of (string * string)
  
type WSState<'a> =
  { websocket : 'a
  ; receivedData : Buffer list
  ; verbose : bool
  }

type Model<'a> =
  { clients : Map<string,WSState<'a>>
  ; membership : Map<string,Set<(string * string)>>
  ; subids : Map<(string * string),string>
  ; broadcast : Broadcast.State<string>
  ; events : Effect list
  }
  
let connectRE = Util.re "^[Cc][Oo][Nn][Nn][Ee][Cc[][Tt]( |$)"
let pubRE = Util.re "^[Pp][Uu][Bb]( |$)"
let subRE = Util.re "^[Ss][Uu][Bb]( |$)"
let unsubRE = Util.re "^[Uu][Nn][Ss][Uu][Bb]( |$)"
let pingRE = Util.re "^[Pp][Ii][Nn][Gg]( |$)"
let pongRE = Util.re "^[Pp][Oo][Nn][Gg]( |$)"
  
let byteSeqFromBuffer b =
  seq {
      for i = 0 to (Buffer.length b) - 1 do
        yield Buffer.at i b
    }

let byteSeqFromBuffers bl =
  Seq.concat (List.map byteSeqFromBuffer bl)

let rec dropBytes n state =
  if n = 0 then
    Some state
  else
    match state.receivedData with
    | hd :: tl ->
       if Buffer.length hd = n then
         Some { state with receivedData = tl }
       else if Buffer.length hd < n then
         let remaining = n - (Buffer.length hd) in
         dropBytes remaining { state with receivedData = tl }
       else
         let newBuf = Buffer.slice n (Buffer.length hd) hd in
         Some { state with receivedData = newBuf :: tl }
    | _ -> None

let bufcat bs =
  bs |> Seq.map Buffer.toArray |> Seq.concat |> Array.ofSeq |> Buffer.fromArray
         
let chopBytes n state =
  let rec chopInner n res state =
    if n = 0 then
      Some (Buffer.fresh 0, state)
    else
      match state.receivedData with
      | hd :: tl ->
         if Buffer.length hd = n then
           Some
             (List.rev (hd :: res) |> bufcat,
              { state with receivedData = tl })
         else if Buffer.length hd < n then
           let remaining = n - (Buffer.length hd) in
           chopInner remaining (hd :: res) { state with receivedData = tl }
         else
           let getBuf = Buffer.slice 0 n hd in
           let newBuf = Buffer.slice n (Buffer.length hd) hd in
           Some
             (List.rev (getBuf :: res) |> bufcat,
              { state with receivedData = newBuf :: tl })
      | _ -> None
  in
  chopInner n [] state
  
let chopFirstLine state =
  let bs = byteSeqFromBuffers state.receivedData in
  let indexOfLinefeed = Seq.tryFindIndex ((=) 10) bs in
  match indexOfLinefeed with
  | Some lf ->
     let firstLineData = Seq.take lf bs |> Seq.filter ((<>) 13) in
     let firstLineBuf = Array.ofSeq firstLineData |> Buffer.fromArray in
     let firstLine = firstLineBuf |> Buffer.toString "utf-8" in
     let _ = printfn "parsing line %A" firstLine in
     dropBytes (lf + 1) state
     |> Option.map (fun state -> (state, firstLine))
  | _ -> None

let clientError wsid emsg state : Model<'a> =
  let fullString = String.concat "" ["-ERR ";emsg;"\r\n"] in
  { state with
      events = (WSSend (wsid, fullString)) :: state.events
  }

let interpretConnectMessage wsid line (state : Model<'a>) : Model<'a> =
  Map.tryFind wsid state.clients
  |> Option.bind
       (fun client -> Serialize.parse line |> Option.map (fun s -> (client,s)))
  |> Option.map
       (fun (client,s) ->
         let nc =
           { client with
               verbose =
                 Serialize.field "verbose" s |> Option.map Serialize.truthy
                 |> optionDefault false
           }
         in
         { state with clients = Map.add wsid client state.clients }
       )
  |> optionDefault (clientError wsid "error parsing connect info" state)

let pong wsid reply state : Model<'a> =
  let fullString =
    if reply = "" then
      "PONG\r\n"
    else
      String.concat "" ["PONG ";reply;"\r\n"]
  in
  let _ = printfn "pong return %A" fullString in
  { state with
      events = (WSSend (wsid, fullString)) :: state.events
  }

let applyBroadcastEff eff state =
  match Util.log "beff" eff with
  | OutPacket (peer,body) ->
     { state with events = (RpcRequest (peer,body)) :: state.events }
  | UserMessage msg ->
     let targets =
       Map.tryFind msg.channel state.membership
       |> optionDefault Set.empty
     in
     let outmsgs =
       targets
       |> Seq.map
            (fun (wsid,sid) ->
              let dataBuf = Buffer.fromString msg.data "utf-8" in
              let fullMsg =
                String.concat
                  ""
                  [ "MSG " ; msg.channel ; " " ; sid ; " "
                  ; string (Buffer.length dataBuf) ; "\r\n" ; msg.data ; "\r\n"
                  ]
              in
              WSSend (wsid, fullMsg)
            )
     in
     Seq.fold
       (fun state evt -> { state with events = evt :: state.events })
       state
       outmsgs
  | BroadcastReady channel ->
     let chankey = BroadcastData.stringKey channel in
     let body = Serialize.jsonObject [| |] in
     (* Emit a throwaway request to trigger a find_node on the channel key,
        which will cause us to drill down to the best masters. *)
     { state with events = (RpcRequest (chankey,body)) :: state.events }
  | _ -> state
  
let doBroadcastMsg msg state =
  let (b,e) = Broadcast.update msg state.broadcast in
  List.fold
    (fun s e -> applyBroadcastEff e s)
    { state with broadcast = b }
    e

let doPublish wsid pub subj replyto bytesStr state =
  let mt = (Util.parseInt bytesStr, Map.tryFind wsid state.clients) in
  let _ = printfn "doPublish1 %A" mt in
  (match mt with
   | (Some n, Some client) -> Some (n,client)
   | _ -> None
  )
  |> Option.bind (fun (n,client) -> chopBytes (n + 2) client)
  |> Option.map
       (fun (buf,client) ->
         doBroadcastMsg
           (InUserMessage (subj,Buffer.toString "utf-8" buf))
           { state with clients = Map.add wsid client state.clients }
       )
  |> optionDefault (clientError wsid "error parsing bytes" state)

let doSubscribe wsid sid subj state =
  let subSet = Map.tryFind subj state.membership |> optionDefault (Set.ofSeq []) in
  let newSet = Set.add (wsid,sid) subSet in
  let state =
    if Set.isEmpty subSet then
      doBroadcastMsg (JoinBroadcast subj) state
    else
      state
  in
  { state with
      subids = Map.add (wsid,sid) subj state.subids ;
      membership = Map.add subj newSet state.membership
  }

let doUnsubscribe (wsid : string) (sid : string) (subj : string) (state : Model<'a>) =
  let subSet = Map.tryFind subj state.membership in
  let newSet =
    subSet
    |> Option.map (Set.remove (wsid,sid))
    |> optionDefault Set.empty
  in
  if Set.isEmpty newSet then
    begin
      let state = doBroadcastMsg (LeaveBroadcast subj) state in
      { state with
          subids = Map.remove (wsid,sid) state.subids ;
          membership = Map.remove subj state.membership
      }
    end
  else
    { state with
        subids = Map.remove (wsid,sid) state.subids ;
        membership = Map.add subj newSet state.membership
    }
  
let runCommand wsid matches firstLine (state : Model<'a>) : Model<'a> =
  match matches with
  | [("connect",true)] ->
     let restString = Util.substr 7 (String.length firstLine) firstLine in
     interpretConnectMessage wsid restString state
  | [("ping",true)] ->
     let restString = Util.substr 5 (String.length firstLine) firstLine in
     pong wsid restString state
  | [("pong",true)] ->
     state
  | [("pub",true)] ->
     match Util.stringSplit " " firstLine with
     | [| pub; subj; bytes |] -> (* No reply-to *)
        doPublish wsid pub subj None bytes state
     | [| pub; subj; replyto; bytes |] -> (* reply-to *)
        doPublish wsid pub subj (Some replyto) bytes state
     | _ -> clientError wsid "pub" state
  | [("sub",true)] ->
     match Util.stringSplit " " firstLine with
     | [| sub; subj; sid |] -> (* No queue-group *)
        doSubscribe wsid sid subj state
     | [| sub; subj; qg; sid |] -> (* No queue-group *)
        doSubscribe wsid sid subj state
     | _ -> clientError wsid "sub" state
  | [("unsub",true)] ->
     match Util.stringSplit " " firstLine with
     | [| unsub; sid |] -> (* No limit *)
        Map.tryFind (wsid,sid) state.subids
        |> Option.map (fun subj -> doUnsubscribe wsid sid subj state)
        |> optionDefault state
     | [| unsub; sid; max |] -> (* Limit *)
        Map.tryFind (wsid,sid) state.subids
        |> Option.map (fun subj -> doUnsubscribe wsid sid subj state)
        |> optionDefault state
     | _ -> clientError wsid "unsub" state
  | _ -> clientError wsid "??" state
  
let rec tryParse wsid client state =
  let _ = printfn "Buffer now %A" client.receivedData in
  (* Find the end of the first line, stupidly for the moment *)
  chopFirstLine client
  |> Option.map
       (fun (client,firstLine) ->
         (* Parse the command line, and see if it needs subsequent data *)
         let matches =
           [ ("connect", connectRE)
           ; ("pub", pubRE)
           ; ("sub", subRE)
           ; ("unsub", unsubRE)
           ; ("ping", pingRE)
           ; ("pong", pongRE)
           ]
           |> List.map (fun (n,m) -> (n,Util.reMatch m firstLine))
           |> List.filter (fun (n,m) -> m)
         in
         let _ = printfn "matches %A" matches in
         let state = { state with clients = Map.add wsid client state.clients } in
         let state = runCommand wsid matches firstLine state in
         Map.tryFind wsid state.clients
         |> Option.map (fun client -> tryParse wsid client state)
         |> optionDefault state
       )
  |> optionDefault state
  
let doWebSocketMsg wsid msgBuf state =
  Map.tryFind wsid state.clients
  |> Option.map
       (fun client -> 
         tryParse
           wsid { client with receivedData = client.receivedData @ [msgBuf] } state
       )
  |> optionDefault state

let newSocket wsid ws model =
  { model with
      clients =
        Map.add
          wsid
          { websocket = ws
          ; receivedData = []
          ; verbose = true
          }
          model.clients ;
      events =
        (WSSend (wsid, "INFO {}\r\n")) :: model.events
  }

let updateSocket id state model =
  { model with clients = Map.add id state model.clients }

let removeSocket id model =
  { model with clients = Map.remove id model.clients }

let init _ : Model<'a> =
  { clients = Map.empty
  ; subids = Map.empty
  ; membership = Map.empty
  ; broadcast = Broadcast.init 3
  ; events = []
  }
  
let update msg state =
  match Util.log "bcast-msg" msg with
  | SetId id ->
     doBroadcastMsg (BroadcastData.SetId id) state
  | AddNode nid ->
     doBroadcastMsg (BroadcastData.AddNode nid) state
  | RemoveNode nid ->
     doBroadcastMsg (BroadcastData.RemoveNode nid) state
  | NewSocket (wsid,ws) ->
     newSocket wsid ws state
  | SocketClosed wsid ->
     removeSocket wsid state
  | WSReceive (wsid,buf) ->
     doWebSocketMsg wsid buf state
  | Tick ->
     let events =
       Map.toSeq state.clients
       |> Seq.map (fun (k,v) -> WSSend (k, "PING\r\n"))
       |> List.ofSeq
     in
     doBroadcastMsg
       BroadcastData.TimeTick { state with events = events @ state.events }
  | RpcSuccess (peer,body) -> state
  | RpcRequestIn (peer,body) ->
     doBroadcastMsg (BroadcastData.InPacket (peer,body)) state
  | _ -> state
