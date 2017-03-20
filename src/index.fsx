#r "../node_modules/fable-core/Fable.Core.dll"

#load "./wrap/util.fs"
#load "./wrap/serialize.fs"
#load "./wrap/buffer.fs"
#load "./wrap/q.fs"
#load "./wrap/dns.fs"
#load "./wrap/ipaddr.fs"
#load "./wrap/network.fs"

open Util

let main _ : unit =
  Network.get_interfaces_list ()
  |> Q.map (fun a -> printfn "interfaces %A" a)
  |> Q.handle (fun e -> printfn "Error %s" (toString e))
  |> Q.fin
