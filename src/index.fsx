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

(* Things to be done (kind of hard) 

 - Wrapper for dht lib (?)
 - Route collection
 - Route traversal
 - Local master election
 - dtls wrapper

 General plan:

 - We use the DHT library mostly unmodified but provide a socket object we control.
   - Our socket object doesn't even represent a network connection so much as a
   - delivery contract.  We don't use real IP addresses.
 - In the presence of a local peer, with nothing else, we bootstrap to them.
 - We store our known peers set at intervals.
 - We load them when we start up, trying to contact a subset.

Every node contains a full network map for now.  It could be improved.

For the purpose of message forwarding, we'll have brief rounds:

- The routing map is fixed for a specific round.
- A message lasts in our queue for a specific number of "good rounds" before expiring.
- A link is marked dead during the round if we send a message and don't get a reply
  within the round.
- We don't send any messages for the last 1/3 of the round time.
- A good round is a round in which the routing map did not change w.r.t the target
  node.

*)



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
