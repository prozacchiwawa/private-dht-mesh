module TopoRun

open Topology
     
let run () =
  let _ = printfn "Starting topology service" in
  let tickbus = Bacon.repeatedly 5000 [| () |] in
  let state = ref (init ()) in
  let rbus = Bacon.newBus () in
  let rob = Bacon.busObservable rbus in
  let _ = 
    rob.onValue
      (fun v ->
        let newState = update v !state in
        state :=
          List.fold
            (fun s e -> passOnEffect outputBus e s)
            { newState with events = [] }
            (List.rev newState.events)
      )
  in
  let _ = tickbus.onValue (fun _ -> rbus.push Tick) in
  let _ = inputBus.onValue (fun msg -> rbus.push msg) in
  
