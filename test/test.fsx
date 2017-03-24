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

let newContact (b : string) : KBucketNode =
  { id = nodeId b }

let kbOps : KBucketAbstract<Buffer,KBucketNode> =
  { distance = KBucket.defaultDistance
  ; nodeId = fun a -> a.id
  ; arbiter = fun (a : KBucketNode) (b : KBucketNode) -> a
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
            let contact = newContact "a" in
            let (kb2,_) = KBucket.add kbOps kb contact None in
            massert.ok (kb2.bucket = Some [|contact|]) ;
            donef ()
      ; "adding an existing contact does not increase number of contacts in bucket" =>
          fun donef ->
            let id = ShortId.generate () in
            let kb = KBucket.init (nodeId id) in
            let contact = newContact "a" in
            let ct2 = newContact "a" in
            let (kb2,_) = KBucket.add kbOps kb contact None in
            let (kb3,_) = KBucket.add kbOps kb2 ct2 None in
            massert.ok ((kb3.bucket |> optionMap Array.length) = Some 1) ;
            donef ()


(*
test['adding same contact moves it to the end of the bucket ' +
     '(most-recently-contacted end)'] = function (test) {
    test.expect(5);
    var kBucket = new KBucket();
    var contact = {id: new Buffer("a")};
    kBucket.add(contact);
    test.equal(kBucket.bucket.length, 1);
    kBucket.add({id: new Buffer("b")});
    test.equal(kBucket.bucket.length, 2);
    test.equal(kBucket.bucket[0], contact); // least-recently-contacted end
    kBucket.add(contact);
    test.equal(kBucket.bucket.length, 2);
    test.equal(kBucket.bucket[1], contact); // most-recently-contacted end
    test.done();
                                                   };

test['adding contact to bucket that can\'t be split results in emitting' +
     ' "ping" event'] = function (test) {
    var i, iString, j;
    test.expect(constants.DEFAULT_NUMBER_OF_NODES_TO_PING + 2);
    var kBucket = new KBucket({localNodeId: new Buffer('0000', 'hex')});
    kBucket.on('ping', function (contacts, replacement) {
        test.equal(contacts.length, constants.DEFAULT_NUMBER_OF_NODES_TO_PING);
        // console.dir(kBucket.high.bucket[0]);
        for (var i = 0; i < constants.DEFAULT_NUMBER_OF_NODES_TO_PING; i++) {
            // the least recently contacted end of the bucket should be pinged
            test.equal(contacts[i], kBucket.high.bucket[i]);
              }
        test.deepEqual(replacement, {id: new Buffer(iString, 'hex')})
        test.done();
                                  });
    for (var j = 0; j < constants.DEFAULT_NUMBER_OF_NODES_PER_K_BUCKET + 1; j++) {
        iString = j.toString('16');
        if (iString.length < 2) {
            iString = '0' + iString;
             }
        iString = '80' + iString; // make sure all go into "far away" bucket
        kBucket.add({id: new Buffer(iString, 'hex')});
          }
                                   };        
 *)
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
