module BroadcastData

open Util

type UserMsg =
  { channel : string ; data : string }

type SubMsg<'peer> =
  { op : 'peer ; peer : 'peer ; channel : string }
  
type PrimaryMsg<'peer> =
  { seq : int ; peer : 'peer ; channel : string ; data : string }

type FwdMsg<'peer> =
  { seq : int
  ; op : 'peer
  ; peer : 'peer
  ; channel : string
  ; data : string
  }

type DelMsg<'peer> = FwdMsg<'peer>

type Message<'peer> =
  | Ping of SubMsg<'peer>
  | Primary of PrimaryMsg<'peer>
  | M2M of FwdMsg<'peer>
  | P2M of FwdMsg<'peer>
  | Delivery of DelMsg<'peer>

type Msg<'peer> =
  | SetId of 'peer
  | TimeTick
  | InPacket of ('peer * Serialize.Json)
  | InUserMessage of (string * string)
  | JoinBroadcast of string
  | LeaveBroadcast of string
  | SetMasters of (string * 'peer list)
                    
type SideEffect<'peer> =
  | BroadcastReady of string
  | OutPacket of ('peer * Serialize.Json)
  | UserMessage of UserMsg

let encodePacket msg =
  match msg with
  | Ping msg ->
     Serialize.jsonObject
       [| ("c", Serialize.jsonString msg.channel)
        ; ("z", Serialize.jsonString msg.op)
       |]
  | Primary msg ->
     Serialize.jsonObject
       [| ("t", Serialize.jsonInt 1)
        ; ("c", Serialize.jsonString msg.channel)
        ; ("m", Serialize.jsonString msg.data)
        ; ("s", Serialize.jsonInt msg.seq)
       |]
  | M2M msg ->
     Serialize.jsonObject
       [| ("t", Serialize.jsonInt 2)
        ; ("c", Serialize.jsonString msg.channel)
        ; ("m", Serialize.jsonString msg.data)
        ; ("s", Serialize.jsonInt msg.seq)
        ; ("z", Serialize.jsonString msg.op)
       |]
  | P2M msg ->
     Serialize.jsonObject
       [| ("t", Serialize.jsonInt 3)
        ; ("c", Serialize.jsonString msg.channel)
        ; ("m", Serialize.jsonString msg.data)
        ; ("s", Serialize.jsonInt msg.seq)
        ; ("z", Serialize.jsonString msg.op)
       |]
  | Delivery msg ->
     Serialize.jsonObject
       [| ("t", Serialize.jsonInt 4)
        ; ("c", Serialize.jsonString msg.channel)
        ; ("m", Serialize.jsonString msg.data)
        ; ("s", Serialize.jsonInt msg.seq)
        ; ("z", Serialize.jsonString msg.op)
       |]

let decodePacket p j =
  let mt = 
    (Serialize.field "c" j |> Option.map Serialize.asString
    , (Serialize.field "m" j |> Option.map Serialize.asString
      , (Serialize.field "s" j |> Option.bind Serialize.floor
        , (Serialize.field "t" j |> Option.bind Serialize.floor
          , Serialize.field "z" j |> Option.map Serialize.asString
          )
        )
      )
    )
  in
  match mt with
  | (Some c, (Some m, (Some s, (Some 1, _)))) ->
     Primary
       { seq = s
       ; channel = c
       ; peer = p
       ; data = m
       } |> Some
  | (Some c, (Some m, (Some s, (Some 2, Some z)))) ->
     M2M
       { seq = s
       ; channel = c
       ; peer = p
       ; data = m
       ; op = z
       } |> Some
  | (Some c, (Some m, (Some s, (Some 3, Some z)))) ->
     P2M
       { seq = s
       ; channel = c
       ; peer = p
       ; data = m
       ; op = z
       } |> Some
  | (Some c, (Some m, (Some s, (Some 4, Some z)))) ->
     Delivery
        { seq = s
        ; channel = c
        ; peer = p
        ; data = m
        ; op = z
        } |> Some
  | (Some c, (_, (_, (_, Some z)))) ->
     Ping { channel = c ; peer = p ; op = z } |> Some
  | _ -> None
    
let stringKey channel =
  let h = Crypto.createHash "sha256" in
  let _ = Crypto.updateBuffer (Buffer.fromString channel "utf-8") h in
  Crypto.digestHex h
