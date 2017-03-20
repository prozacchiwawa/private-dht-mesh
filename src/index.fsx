#r "../node_modules/fable-core/Fable.Core.dll"

#load "./wrap/util.fs"
#load "./wrap/serialize.fs"
#load "./wrap/buffer.fs"
#load "./wrap/q.fs"
#load "./wrap/dns.fs"
#load "./wrap/ipaddr.fs"
#load "./wrap/network.fs"
#load "./wrap/bonjour.fs"

open Util

let main _ : unit =
  let _ =
    Network.get_interfaces_list ()
    |> Q.map (fun a -> printfn "interfaces %A" a)
    |> Q.handle (fun e -> printfn "Error %s" (toString e))
    |> Q.fin
  in
  let bonjour = Bonjour.newBonjour () in
  begin
    bonjour.publish (Bonjour.serviceDesc "testfs" "http" 3000) ;
    Bonjour.find (Bonjour.serviceQueryByType "http") (printfn "Service %A") bonjour
  end
