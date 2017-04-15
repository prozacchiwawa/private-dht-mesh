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
  }

let init nodes endpoints delays =
  { clock = 0
  ; nodes = nodes
  ; endpoints = endpoints
  ; delays = delays
  ; inFlightDatagrams = Map.empty
  ; events = []
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
    
let propEvents isDatagram passOn self =
  let (datagrams,oldEvents) = List.partition (fun e -> isDatagram e) self.events in
  List.fold
    (fun self dg -> addDatagram (passOn dg) self)
    { self with events = oldEvents }
    datagrams

let map harvest isDatagram passOn f nid self =
  self.nodes
  |> Map.tryFind nid
  |> optionMap
       (fun n ->
         let updated = f n in
         let (events,node) = harvest updated in
         { self with
             nodes =
               Map.add nid updated self.nodes ;
             events = events @ self.events
         }
         |> propEvents isDatagram passOn
       )
  |> optionDefault self
                   
let mapall harvest isDatagram passOn f self =
  let newNodes =
    Map.toSeq self.nodes
    |> Seq.map (fun (k,v) -> harvest (f v))
    |> Seq.toList
  in
  let newEvents = newNodes |> List.map (fun (k,(e,v)) -> e) in
  let newNodes = newNodes |> List.map (fun (k,(e,v)) -> (k,v)) in
  propEvents
    isDatagram
    passOn
    { self with nodes = Map.ofSeq newNodes ; events = newEvents @ self.events }
    
let tick sendFn harvest isDatagram passOn self =
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
               let (events,node) = harvest nid unode in
               { self with
                   nodes = Map.add nid node self.nodes ;
                   events = events @ self.events
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
  propEvents
    isDatagram
    passOn
    self
