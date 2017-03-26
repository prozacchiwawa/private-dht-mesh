#r "../node_modules/fable-core/Fable.Core.dll"

#load "../src/wrap/util.fs"
#load "../src/wrap/serialize.fs"
#load "../src/wrap/buffer.fs"
#load "../src/wrap/shortid.fs"
#load "../src/wrap/q.fs"
#load "../src/wrap/dns.fs"
#load "../src/wrap/ipaddr.fs"
#load "../src/wrap/network.fs"
#load "../src/wrap/bonjour.fs"
#load "../src/wrap/crypto.fs"
#load "../src/queue.fs"
#load "../src/forward.fs"
#load "../src/kbucket.fs"
#load "./mocha.fs"

open Util
open Buffer
open MochaTest
open Forward
open KBucket

type DoneF = unit -> unit
type It = string * (DoneF -> unit)

let (=>) (a : string) (b : 'b) : (string * 'b) = (a,b)

let basicNodeA = { local = true ; target = "169.254.1.2" ; id = "testA" }
let basicNodeB = { local = true ; target = "169.254.1.3" ; id = "testB" }

let final f _ = f ()

let nodeId (b : string) : Buffer =
  let hasher = Crypto.createHash "sha256" in
  let bBuf = Buffer.fromString b "utf-8" in
  let _ = Crypto.updateBuffer bBuf hasher in
  let digest = Crypto.digestBuffer hasher in
  Buffer.slice 0 20 digest

type KBucketNode = { id : Buffer }

let newContactString (b : string) : KBucketNode =
  { id = nodeId b }

let newContactBuffer (b : Buffer) : KBucketNode =
  { id = b }

let kbOps : KBucketAbstract<Buffer,KBucketNode> =
  { distance = KBucket.defaultDistance
  ; nodeId = fun a -> a.id
  ; arbiter = fun (a : KBucketNode) (b : KBucketNode) -> b
  ; keyLength = Buffer.length
  ; keyNth = Buffer.at
  ; idEqual = Buffer.equal
  }

let tests : (string * (It list)) list =
  [ "forward" => 
      [ "should be creatable" =>
          fun donef -> let i = Forward.init () in donef ()
      ; "should allow addition of nodes" =>
          fun donef ->
            let i = Forward.init () in
            begin
              i |> Forward.addNode basicNodeA |> final donef
            end
      ; "should allow addition of edges" =>
          fun donef ->
            let i = Forward.init () in
            begin
              i
              |> Forward.addNode basicNodeA
              |> Forward.addNode basicNodeB
              |> Forward.addEdge basicNodeA.id basicNodeB.id
              |> final donef 
            end
      ; "should know if two nodes are connected" =>
          fun donef ->
            let i =
              Forward.init ()
              |> Forward.addNode basicNodeA
              |> Forward.addNode basicNodeB
              |> Forward.addEdge basicNodeA.id basicNodeB.id
            in
            begin
              massert.ok (i |> Forward.connected basicNodeA.id basicNodeB.id) ;
              massert.ok
                (i 
                 |> Forward.removeEdge basicNodeA.id basicNodeB.id
                 |> Forward.connected basicNodeA.id basicNodeB.id
                 |> not
                ) ;
              donef ()
            end
      ]
  ; "k-bucket" =>
      [ "adding a contact places it in a bucket" =>
          fun donef ->
            let id = ShortId.generate () in
            let kb = KBucket.init (nodeId id) in
            let contact = newContactString "a" in
            let (kb2,_) = KBucket.add kbOps kb contact None in
            massert.ok (kb2.bucket = Some [|contact|]) ;
            donef ()
      ; "adding an existing contact does not increase number of contacts in bucket" =>
          fun donef ->
            let id = ShortId.generate () in
            let kb = KBucket.init (nodeId id) in
            let contact = newContactString "a" in
            let ct2 = newContactString "a" in
            let (kb2,_) = KBucket.add kbOps kb contact None in
            let (kb3,_) = KBucket.add kbOps kb2 ct2 None in
            massert.ok ((kb3.bucket |> optionMap Array.length) = Some 1) ;
            donef ()

      ; "adding same contact moves it to the end of the bucket (most-recently-contacted end)" =>
          fun donef ->
            let id = ShortId.generate () in
            let kb = KBucket.init (nodeId id) in
            let contact = newContactString "a" in
            let (kb2,_) = KBucket.add kbOps kb contact None in
            let _ = massert.ok ((kb2.bucket |> optionMap Array.length) = Some 1) in
            let (kb3,_) = KBucket.add kbOps kb2 (newContactString "b") None in
            let _ = massert.ok ((kb3.bucket |> optionMap Array.length) = Some 2) in
            let _ =
              massert.ok
                ((kb3.bucket |> optionMap (fun a -> a.[0])) =
                   (Some contact))
            in
            let (kb4,_) = KBucket.add kbOps kb3 contact None in
            let _ = massert.ok ((kb4.bucket |> optionMap Array.length) = Some 2) in
            let _ =
              massert.ok
                ((kb4.bucket |> optionMap (fun a -> a.[1])) =
                   (Some contact))
            in
            donef ()

      ; "adding contact to bucket that can\'t be split results in emitting \"ping\" event" =>
          fun donef ->
            let id = Buffer.fromArray [| 0; 0 |] in
            let kb = ref (KBucket.init id) in
            let pings = ref [| |] in
            for j = 0 to KBucket.Constants.DEFAULT_NUMBER_OF_NODES_PER_K_BUCKET do
              begin
                let iString = [| 0x80; j |] in
                let (kbu,p) = KBucket.add kbOps !kb (newContactBuffer (Buffer.fromArray iString)) None in
                kb := kbu ;
                pings := Seq.concat [p |> List.toSeq;!pings |> Array.toSeq] |> Array.ofSeq ;
              end ;
            let _ =
              match !pings with
              | [|Ping (contacts, replacement)|] ->
                 massert.ok ((Seq.length contacts) = KBucket.Constants.DEFAULT_NUMBER_OF_NODES_TO_PING)
              | _ -> massert.fail true
            in
            !pings
            |> Seq.mapi
                 (fun i a ->
                   match (a, (!kb).high) with
                   | (Ping (contacts, replacement), Some high) ->
                      begin
                        let iString = [| 0x80; i |] in
                        match high.bucket with
                        | Some bucket ->
                           massert.ok ((Array.ofSeq contacts).[i] = bucket.[i]) ;
                           massert.ok 
                             (newContactBuffer (Buffer.fromArray iString) =
                                replacement) ;
                        | _ -> (massert.ok false ; donef ())
                      end
                   | _ -> (massert.ok false ; donef ())
                 )
            |> (fun _ -> donef ())
      ]
  ]

let _ =
  List.map
    (fun (n,t) ->
      describe
        n
        (fun () ->
          List.map (fun (itName,itTest) -> it itName itTest) t
          |> ignore
        )
    )
    tests
