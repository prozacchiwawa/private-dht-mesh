module BroadcastData

open Util
   
type ForwardKind = Originated | Forwarded of string | Received
   
type Message =
  { channel : string
  ; seq : int
  ; data : string option
  }

type SideEffect<'peer> =
  | OutPacket of ('peer * Serialize.Json)
  | UserMessage of Message

let stringKey channel =
  let h = Crypto.createHash "sha256" in
  let _ = Crypto.updateBuffer (Buffer.fromString channel "utf-8") h in
  Crypto.digestHex h

let encodePacketForChannel msg forwarded =
  Serialize.jsonObject
    (List.concat
       [ [ ("c", Serialize.jsonString msg.channel)
         ; ("s", Serialize.jsonInt msg.seq)
         ]
       ; msg.data
         |> Option.map (fun buf -> ("m", Serialize.jsonString buf))
         |> optionToList
       ; forwarded
         |> Option.map (fun from -> ("f", Serialize.jsonString from))
         |> optionToList
       ] |> Array.ofSeq
    )

let makePacket (msg : Message) (p : string) (forward : string option) =
  [OutPacket (p,encodePacketForChannel msg forward)]
