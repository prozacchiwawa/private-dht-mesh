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
  | Do of BroadcastData.Msg<string>

type BIter =
  { broadcast : Broadcast.State<string>
  ; events : SideEffect<string> list
  }
            
let rec runIter (l : IterateAction list) (b : BIter) : BIter =
  match l with
  | [] -> b
  | (Wait 0) :: tl -> runIter tl b
  | (Wait n) :: tl ->
     let _ = printfn "%A -> peers of foo: %A" b.broadcast.myId (Broadcast.peersOfChannel "foo" b.broadcast) in
     let (newb,beff) = Broadcast.update (BroadcastData.TimeTick) b.broadcast in
     let _ = printfn "%A <- peers of foo: %A" newb.myId (Broadcast.peersOfChannel "foo" newb) in
     runIter
       ((Wait (n-1)) :: tl)
       { b with broadcast = newb ; events = beff @ b.events }
  | (Do msg) :: tl ->
     let _ = printfn "%A -> peers of foo: %A" b.broadcast.myId (Broadcast.peersOfChannel "foo" b.broadcast) in
     let (newb,beff) = Broadcast.update msg b.broadcast in
     let _ = printfn "%A eff %A" newb.myId beff in
     let _ = printfn "%A <- peers of foo: %A" newb.myId (Broadcast.peersOfChannel "foo" newb) in
     runIter
       tl
       { b with broadcast = newb ; events = beff @ b.events }

let startIter broadcast = { broadcast = broadcast ; events = [] }

let rec applyEvent from newmsgs results bev =
  match bev with
  | (UserMessage m) :: tl -> applyEvent from newmsgs (m :: results) tl
  | (OutPacket (peer,data)) :: tl ->
     let _ = printfn "DG %A->%A %A" from peer data in
     match Serialize.field "c" data |> Option.map Serialize.asString with
     | Some c ->
        let nev =
          List.concat
            [ [ (peer, Do (InPacket (from,data)))
              ]
            ; newmsgs
            ]
        in
        applyEvent from nev results tl
     | _ -> applyEvent from newmsgs results tl
  | _ :: tl ->
     applyEvent from newmsgs results tl
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
        let bnext = { bnew with events = [] } in
        let (newmsgs,results) = applyEvent tgt [] results bnew.events in
        let m = beforeTick (List.rev newmsgs) tl in
        let nbroadcasts = Map.add tgt bnext broadcasts in
        applyMessages m results nbroadcasts
     | None -> applyMessages tl results broadcasts
  | _ -> (results, broadcasts)

let tests : Test list =
  [ "should be creatable" =>
      fun donef -> let i = Broadcast.init 3 in donef ()
  ; "should send a datagram when we join a channel" =>
      fun donef ->
        let ourNode = BroadcastData.stringKey "our-node" in
        let theirNode = BroadcastData.stringKey "their-node" in
        let res =
          Broadcast.init 3
          |> startIter
          |> runIter
               [ Do (SetId ourNode)
               ; Do (SetMasters ("foo",[theirNode]))
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
        let keys =
          seq { 0..2 }
          |> Seq.map string
          |> Seq.map BroadcastData.stringKey
          |> Array.ofSeq
        in
        let broadcasts =
          [ ( keys.[0]
            , Broadcast.init 100
              |> startIter
              |> runIter
                   [ Do (SetId keys.[0])
                   ; Do (JoinBroadcast "foo")
                   ; Do (SetMasters ("foo",[keys.[0]]))
                   ]
            )
          ; ( keys.[1]
            , Broadcast.init 100
              |> startIter
              |> runIter
                   [ Do (SetId keys.[1])
                   ; Do (JoinBroadcast "foo")
                   ; Do (SetMasters ("foo",[keys.[0]]))
                   ]
            )
          ; ( keys.[2]
            , Broadcast.init 100
              |> startIter
              |> runIter
                   [ Do (SetId keys.[2])
                   ; Do (JoinBroadcast "foo")
                   ; Do (SetMasters ("foo",[keys.[0]]))
                   ]
            )
          ] |> Map.ofSeq |> ref
        in
        let results = ref [] in
        for i in 0 .. 299 do
          let msgs =
            if (i - 20) % 100 = 0 then
              [ (keys.[0], Do (InUserMessage ("foo", sprintf "0 %d" i)))
              ; (keys.[1], Do (InUserMessage ("foo", sprintf "1 %d" i)))
              ; (keys.[2], Do (InUserMessage ("foo", sprintf "2 %d" i)))
              ; (keys.[0], Wait 1)
              ; (keys.[1], Wait 1)
              ; (keys.[2], Wait 1)
              ]
            else
              [(keys.[0], Wait 1); (keys.[1], Wait 1); (keys.[2], Wait 1)]
          in
          let (r,b) = applyMessages msgs !results !broadcasts in
          results := r ;
          broadcasts := b
        done ;
        let messagesList =
          !results
          |> List.map (fun m -> m.data)
        in
        let messagesSet = messagesList |> Set.ofSeq in
        let wantMessages =
          ["0 20";"1 20";"2 20";"0 120";"1 120";"2 120";"0 220";"1 220";"2 220"]
          |> Set.ofSeq
        in
        let slen = Seq.length messagesSet in
        let mlen = List.length messagesList in
        let _ = printfn "results %A" messagesList in
        let _ = massert.ok ((3 * slen) = mlen) in
        let _ = massert.ok (messagesSet = wantMessages) in
        donef ()
  ; "should broadcast a datagram to 6 peers under ideal conditions" =>
      fun donef ->
        let keys =
          seq { 0..5 }
          |> Seq.map string
          |> Seq.map BroadcastData.stringKey
          |> Array.ofSeq
        in
        let broadcasts =
          [ ( keys.[0]
            , Broadcast.init 100
              |> startIter
              |> runIter
                   [ Do (SetId keys.[0])
                   ; Do (SetMasters ("foo",[keys.[2];keys.[3]]))
                   ; Do (JoinBroadcast "foo")
                   ]
            )
          ; ( keys.[1]
            , Broadcast.init 100
              |> startIter
              |> runIter
                   [ Do (SetId keys.[1])
                   ; Do (SetMasters ("foo",[keys.[2];keys.[3]]))
                   ; Do (JoinBroadcast "foo")
                   ]
            )
          ; ( keys.[2]
            , Broadcast.init 100
              |> startIter
              |> runIter
                   [ Do (SetId keys.[2])
                   ; Do (SetMasters ("foo",[keys.[2];keys.[3]]))
                   ; Do (JoinBroadcast "foo")
                   ]
            )
          ; ( keys.[3]
            , Broadcast.init 100
              |> startIter
              |> runIter
                   [ Do (SetId keys.[3])
                   ; Do (SetMasters ("foo",[keys.[2];keys.[3]]))
                   ; Do (JoinBroadcast "foo")
                   ]
            )
          ; ( keys.[4]
            , Broadcast.init 100
              |> startIter
              |> runIter
                   [ Do (SetId keys.[4])
                   ; Do (SetMasters ("foo",[keys.[2];keys.[3]]))
                   ; Do (JoinBroadcast "foo")
                   ]
            )
          ; ( keys.[5]
            , Broadcast.init 100
              |> startIter
              |> runIter
                   [ Do (SetId keys.[5])
                   ; Do (SetMasters ("foo",[keys.[2];keys.[3]]))
                   ; Do (JoinBroadcast "foo")
                   ]
            )
          ] |> Map.ofSeq |> ref
        in
        let results = ref [] in
        for i in 0 .. 299 do
          let msgs =
            if (i - 20) % 100 = 0 then
              [ (keys.[0], Do (InUserMessage ("foo", sprintf "0 %d" i)))
              ; (keys.[1], Do (InUserMessage ("foo", sprintf "1 %d" i)))
              ; (keys.[2], Do (InUserMessage ("foo", sprintf "2 %d" i)))
              ; (keys.[3], Do (InUserMessage ("foo", sprintf "3 %d" i)))
              ; (keys.[4], Do (InUserMessage ("foo", sprintf "4 %d" i)))
              ; (keys.[5], Do (InUserMessage ("foo", sprintf "5 %d" i)))
              ; (keys.[0], Wait 1)
              ; (keys.[1], Wait 1)
              ; (keys.[2], Wait 1)
              ; (keys.[3], Wait 1)
              ; (keys.[4], Wait 1)
              ; (keys.[5], Wait 1)
              ]
            else
              [(keys.[0], Wait 1); (keys.[1], Wait 1); (keys.[2], Wait 1);
               (keys.[3], Wait 1); (keys.[4], Wait 1); (keys.[5], Wait 1)]
          in
          let (r,b) = applyMessages msgs !results !broadcasts in
          results := r ;
          broadcasts := b
        done ;
        let messagesList =
          !results
          |> List.map (fun m -> m.data)
        in
        let messagesSet = Set.ofSeq messagesList in
        let wantMessages =
          ["0 20";"1 20";"2 20";"0 120";"1 120";"2 120";"0 220";"1 220";"2 220";
           "3 20";"4 20";"5 20";"3 120";"4 120";"5 120";"3 220";"4 220";"5 220"]
          |> Set.ofSeq
        in
        let gotlen = List.length messagesList in
        let setlen = Seq.length messagesSet in
        let _ = printfn "results: got %A want %A" gotlen setlen in
        let _ = printfn "raw-recv: %A" messagesList in
        let _ =
          massert.ok ((6 * (Seq.length messagesSet)) = (List.length messagesList))
        in
        let _ = massert.ok (messagesSet = wantMessages) in
        donef ()
  ]
