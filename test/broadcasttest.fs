module BroadcastTest

open Util
open Buffer
open MochaTest
open DHTData
open Broadcast
open BroadcastData
   
type DoneF = unit -> unit
type Test = string * (DoneF -> unit)

let (=>) (a : string) (b : 'b) : (string * 'b) = (a,b)

type IterateAction =
  | Wait of int
  | Do of Broadcast.Msg<string>

type BIter =
  { broadcast : Broadcast.State<string>
  ; events : SideEffect<string> list
  }
            
let rec runIter (l : IterateAction list) (b : BIter) : BIter =
  match l with
  | [] -> b
  | (Wait 0) :: tl -> runIter tl b
  | (Wait n) :: tl ->
     let (newb,beff) = Broadcast.update (Broadcast.TimeTick) b.broadcast in
     runIter
       ((Wait (n-1)) :: tl)
       { b with broadcast = newb ; events = beff @ b.events }
  | (Do msg) :: tl ->
     let (newb,beff) = Broadcast.update msg b.broadcast in
     runIter
       tl
       { b with broadcast = newb ; events = beff @ b.events }

let startIter broadcast = { broadcast = broadcast ; events = [] }

let rec applyEvents from newmsgs results bev =
  match bev with
  | (UserMessage m) :: tl -> applyEvents from newmsgs (m :: results) tl
  | (OutPacket (peer,data)) :: tl ->
     let _ = printfn "DG %s->%s %A" from peer data in
     let mt =
       ( Serialize.field "c" data |> Option.map Serialize.asString
       , Serialize.field "s" data |> Option.bind Serialize.floor
       , Serialize.field "m" data |> Option.map Serialize.asString
       )
     in
     match mt with
     | (Some c, Some s, d) ->
        let msg = { channel = c ; seq = s ; data = d } in
        applyEvents
          from
          (List.concat
             [ newmsgs
             ; [ (peer, Do (InMessage (from,msg)))
               ; (from, Do (Success (peer,msg.channel,msg.seq)))
               ]
             ]
          ) results tl
     | _ -> applyEvents from newmsgs results tl
  | _ ->
     (newmsgs,results)

let rec beforeTick newmsgs msgs =
  match msgs with
  | (tgt,Wait n) :: tl -> (newmsgs @ [(tgt,Wait n)] @ tl)
  | (tgt,msg) :: tl -> (tgt,msg) :: (beforeTick newmsgs tl)
  | [] -> newmsgs
       
(* Until msgs is empty, apply each message to the indicated recipient,
 * then iterate.  Collect OutMessage contents in results.
 *)
let rec applyMessages msgs results broadcasts =
  match msgs with
  | (tgt,msg) :: tl ->
     match Map.tryFind tgt broadcasts with
     | Some b ->
        let bnew = runIter [msg] b in
        let bev = bnew.events in
        let bnext = { bnew with events = [] } in
        let (newmsgs,results) = applyEvents tgt [] results bev in
        let newmsgs = beforeTick newmsgs tl in
        applyMessages newmsgs results broadcasts
     | None -> applyMessages tl results broadcasts
  | _ -> (results, broadcasts)
                        
let tests : Test list =
  [ "should be creatable" =>
      fun donef -> let i = Broadcast.init 3 in donef ()
  ; "should send a datagram when we join a channel" =>
      fun donef ->
        let res =
          Broadcast.init 3
          |> startIter
          |> runIter
               [ Do (SetId "our-node")
               ; Do (AddNode "other-node")
               ; Wait 1
               ; Do (JoinBroadcast "foo")
               ; Wait 5
               ]
        in
        let packets =
          List.rev res.events
          |> List.map
               (fun eff ->
                 match eff with
                 | OutPacket p -> [p]
                 | _ -> []
               )
          |> List.concat
        in
        let _ = massert.ok (List.length packets > 0) in
        donef ()
  ; "should broadcast a datagram to 3 peers under ideal conditions" =>
      fun donef ->
        let broadcasts =
          [
            ( "0"
            , Broadcast.init 100
              |> startIter
              |> runIter
                   [ Do (SetId "0")
                   ; Do (AddNode "1")
                   ; Do (AddNode "2")
                   ; Do (JoinBroadcast "foo")
                   ]
            )
          ; ( "1"
            , Broadcast.init 100
              |> startIter
              |> runIter
                   [ Do (SetId "1")
                   ; Do (AddNode "0")
                   ; Do (AddNode "2")
                   ; Do (JoinBroadcast "foo")
                   ]
            )
          ; ( "2"
            , Broadcast.init 100
              |> startIter
              |> runIter
                   [ Do (SetId "2")
                   ; Do (AddNode "0")
                   ; Do (AddNode "1")
                   ; Do (JoinBroadcast "foo")
                   ]
            )
          ] |> Map.ofSeq |> ref
        in
        let results = ref [] in
        for i in 0 .. 299 do
          let msgs =
            if (i - 20) % 100 = 0 then
              [ ("0", Do (OutMessage { channel = "foo"; seq = -1 ; data = Some (sprintf "0 %d" i) }))
              ; ("1", Do (OutMessage { channel = "foo"; seq = -1 ; data = Some (sprintf "1 %d" i) }))
              ; ("2", Do (OutMessage { channel = "foo"; seq = -1 ; data = Some (sprintf "2 %d" i) }))
              ; ("0", Wait 1)
              ; ("1", Wait 1)
              ; ("2", Wait 1)
              ]
            else
              [("0", Wait 1); ("1", Wait 1); ("2", Wait 1)]
          in
          let (r,b) = applyMessages msgs !results !broadcasts in
          results := r ;
          broadcasts := b
        done ;
        let _ = printfn "results %A" !results in
        let messagesList =
          !results
          |> List.map (fun m -> m.data |> optionDefault "")
        in
        let messagesSet = Set.ofSeq messagesList in
        let wantMessages =
          ["0 20";"1 20";"2 20";"0 120";"1 120";"2 120";"0 220";"1 220";"2 220"]
          |> Set.ofSeq
        in
        let _ =
          massert.ok ((3 * (Seq.length messagesSet)) = (List.length messagesList))
        in
        let _ = massert.ok (messagesSet = wantMessages) in
        donef ()
  ]
