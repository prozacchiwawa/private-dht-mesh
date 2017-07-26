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
#load "./queue.fs"
#load "./forward.fs"
#load "./topology.fs"

let stepsToMessage res =
  let steps =
    Seq.map
      (fun (h : Network.TraceHop) -> { Topology.host = h.ip ; Topology.rtt = h.rtt1 })
      res
    |> List.ofSeq
  in
  Topology.EndTrace ("superheterodyne.net",steps)
  
let main _ =
  let topo = Topology.init in
  Network.trace "superheterodyne.net"
  |> Q.map (fun res -> printfn "%A" res ; res)
  |> Q.map
       (fun res ->
         topo
         |> Topology.update (Topology.AddNode "superheterodyne.net")
         |> Topology.update (Topology.AddNode "google.com")
         |> Topology.update (stepsToMessage res)
       )
  |> Q.andThen
       (fun t ->
         Network.trace "86.75.30.9"
         |> Q.map
              (fun res ->
                let _ = printfn "%A" res in
                Topology.update (stepsToMessage res) t
              )
       )
  |> Q.map (fun t -> printfn "topo %A" t)
  |> Q.fin
 
let _ = main ()
