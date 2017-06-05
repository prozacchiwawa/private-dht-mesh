module BonjourService

open Util
open Buffer
open Bacon
open DHTData
open DHTRPC
open DHTRunner
     
let serve dhtid (requestBus : bacon.Bus<DHTRunner.InputEventDHT,unit>) =
  let bonjour = Bonjour.newBonjour () in
  let dhtidArray = Buffer.toArray dhtid in
  let dhtidStr = String.concat "" (Array.map (sprintf "%02x") dhtidArray) in
  let serviceName = String.concat "." ["com.euso.DHTRPC";dhtidStr] in
  bonjour.publish
    (Bonjour.serviceDesc serviceName "com.euso.DHTRPC" Ports.udpport) ;
  Bonjour.find
    (Bonjour.serviceQueryByType "com.euso.DHTRPC")
    (fun service ->
      let host =
        (match service.addresses |> List.ofSeq with
         | hd :: _ -> hd
         | [] -> service.fqdn
        )
      in
      let port = Ports.udpport in
      let fqdn = stringSplit "." service.fqdn in
      if Array.length fqdn > 3 &&
           fqdn.[0] = "com" &&
             fqdn.[1] = "euso" &&
               fqdn.[2] = "DHTRPC"
      then
        let idStr = fqdn.[3] in
        let _ = printfn "fqdn %s from %A" idStr fqdn in
        let id = Buffer.fromString idStr "hex" in
        requestBus.push
          (DHTRunner.AddNode
             { NodeIdent.id = id
             ; NodeIdent.host = host
             ; NodeIdent.port = port
             }
          )
    )
    bonjour ;
  requestBus.push Start
  
