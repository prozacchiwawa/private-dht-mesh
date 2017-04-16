module TestNet

open Util
open Buffer
         
type Datagram =
  { source : string * int
  ; dest : string * int
  ; body : Buffer
  }

type TestNet<'a,'o> =
  { clock : int
  ; inFlightDatagrams : Map<int, Datagram list>
  ; endpoints : Map<(string * int), string>
  ; delays : Map<((string * int) * (string * int)), int>
  ; nodes : Map<string, 'o>
  ; events : 'a list
  ; datagrams : Datagram list
  }

let init nodes endpoints delays =
  { clock = 0
  ; nodes = nodes
  ; endpoints = endpoints
  ; delays = delays
  ; inFlightDatagrams = Map.empty
  ; events = []
  ; datagrams = []
  }

let addDatagram dg self =
  let targetTick =
    Map.tryFind (dg.source,dg.dest) self.delays |> optionDefault (self.clock + 1)
  in
  if targetTick < 1 then
    self
  else
    let dglist =
      self.inFlightDatagrams |> Map.tryFind targetTick |> optionDefault []
    in
    { self with
        inFlightDatagrams =
          Map.add targetTick (dg :: dglist) self.inFlightDatagrams
    }

let propDatagrams self =
  List.fold
    (fun self dg -> addDatagram dg self)
    { self with datagrams = [] }
    (List.rev self.datagrams)

let map harvest f nid self =
  self.nodes
  |> Map.tryFind nid
  |> optionMap
       (fun n ->
         let updated = f n in
         let (events,datagrams,node) = harvest updated in
         { self with
             nodes =
               Map.add nid updated self.nodes ;
             events = events @ self.events ;
             datagrams = datagrams @ self.datagrams
         }
         |> propDatagrams
       )
  |> optionDefault self
                   
let mapall harvest f self =
  let newNodes =
    Map.toSeq self.nodes
    |> Seq.map (fun (k,v) -> harvest (f v))
    |> Seq.toList
  in
  let newNodes = newNodes |> List.map (fun (k,(e,d,v)) -> (k,v)) in
  let newEvents = newNodes |> List.map (fun (k,(e,d,v)) -> e) in
  let newDatagrams = newNodes |> List.map (fun (k,(e,d,v)) -> d) in
  propDatagrams
    { self with
        nodes = Map.ofSeq newNodes ;
        events = newEvents @ self.events ;
        datagrams = newDatagrams @ self.datagrams
    }
    
let tick sendFn harvest self =
  let thisTick = self.clock + 1 in
  let toSend =
    self.inFlightDatagrams |> Map.tryFind thisTick |> optionDefault []
  in
  let self =
    List.fold
      (fun self datagram ->
        self.endpoints
        |> Map.tryFind datagram.dest
        |> optionThen
             (fun nid ->
               Map.tryFind nid self.nodes |> optionMap (fun node -> (nid,node))
             )
        |> optionMap
             (fun (nid,node) ->
               let unode = sendFn datagram.source datagram.body node in
               let (events,datagrams,node) = harvest nid unode in
               { self with
                   nodes = Map.add nid node self.nodes ;
                   events = events @ self.events ;
                   datagrams = datagrams @ self.datagrams
               }
             )
           |> optionDefault self
      )
      { self with
          clock = thisTick ;
          inFlightDatagrams = Map.remove thisTick self.inFlightDatagrams
      }
      toSend
  in
  propDatagrams
    self

let harvest self =
  (List.rev self.events, { self with events = [] })
     
