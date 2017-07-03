#r "../node_modules/fable-core/Fable.Core.dll"

#load "./wrap/util.fs"
#load "./wrap/serialize.fs"
#load "./wrap/buffer.fs"
#load "./wrap/q.fs"
#load "./wrap/dns.fs"
#load "./wrap/ipaddr.fs"
#load "./wrap/network.fs"
#load "./wrap/bonjour.fs"
#load "./wrap/nodesocket.fs"
#load "./wrap/crypto.fs"
#load "./wrap/shortid.fs"
#load "./wrap/bacon.fs"
#load "./wrap/websocket.fs"
#load "./wrap/express.fs"
#load "./wrap/rbtree.fs"
#load "./ports.fs"
#load "./dhtdata.fs"
#load "./kbucket.fs"
#load "./dht.fs"
#load "./dhtrpc.fs"
#load "./elm-return/respond.fs"
#load "./elm-return/return.fs"
#load "./broadcastdata.fs"
#load "./broadcastinstance.fs"
#load "./broadcast.fs"
#load "./dhtrunner.fs"
#load "./basicwsservice.fs"
#load "./bonjourservice.fs"
#load "./bcastrunner.fs"
#load "./broadcastservice.fs"

let main _ =
  Network.trace "superheterodyne.net"
  |> Q.map (fun res -> printfn "%A" res)
  |> Q.fin
 
let _ = main ()
