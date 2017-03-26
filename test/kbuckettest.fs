module KBucketTest

open MochaTest
open Buffer
open KBucket

let (=>) (a : string) (b : 'b) : (string * 'b) = (a,b)

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
  ; idLess = fun a b -> Buffer.compare a b < 0
  }

let tests =
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
  ; "closest nodes are returned" =>
      fun donef ->
        let id = ShortId.generate () in
        let kb = ref (KBucket.init (nodeId id)) in
        let pings = ref [] in
        for i = 0 to 0x11 do
          begin
            let (kbu,pu) =
              KBucket.add kbOps !kb (newContactBuffer (Buffer.fromArray [|i|])) None
            in
            kb := kbu ;
            pings := !pings @ pu
          end ;
        let contact = (newContactBuffer (Buffer.fromArray [|0x15|])) in
        let contacts = KBucket.closest kbOps !kb contact 3 None in
        massert.ok (contacts.[0].id = (Buffer.fromArray [|0x11|])) ;
        massert.ok (contacts.[1].id = (Buffer.fromArray [|0x10|])) ;
        massert.ok (contacts.[2].id = (Buffer.fromArray [|0x05|])) ;
        donef ()

  ; "closest nodes are returned (including exact match)" =>
      fun donef ->
        let id = ShortId.generate () in
        let kb = ref (KBucket.init (nodeId id)) in
        let pings = ref [] in
        for i = 0 to 0x11 do
          begin
            let (kbu,pu) =
              KBucket.add kbOps !kb (newContactBuffer (Buffer.fromArray [|i|])) None
            in
            kb := kbu ;
            pings := !pings @ pu
          end ;
        let contact = (newContactBuffer (Buffer.fromArray [|0x11|])) in
        let contacts = KBucket.closest kbOps !kb contact 3 None in
        massert.ok (contacts.[0].id = (Buffer.fromArray [|0x11|])) ;
        massert.ok (contacts.[1].id = (Buffer.fromArray [|0x10|])) ;
        massert.ok (contacts.[2].id = (Buffer.fromArray [|0x01|])) ;
        donef ()
(*
test['closest nodes are returned even if there isn\'t enough in one bucket'] = function (test) {
    test.expect(22);
    var i, iString;
    var kBucket = new KBucket({localNodeId: new Buffer('0000', 'hex')});
    for (i = 0; i < constants.DEFAULT_NUMBER_OF_NODES_PER_K_BUCKET; i++) {
        iString = i.toString('16');
        if (iString.length < 2) {
            iString = '0' + iString;
          }
        var farString = '80' + iString; // make sure all go into "far away" bucket
        kBucket.add({id: new Buffer(farString, 'hex')});
        var nearString = '01' + iString;
        kBucket.add({id: new Buffer(nearString, 'hex')});
                                                                                          }
    kBucket.add({id: new Buffer('0001', 'hex')});
    var contact = {id: new Buffer('0003', 'hex')}; // 0000000000000011
    var contacts = kBucket.closest(contact, 22);
    test.deepEqual(contacts[0].id, new Buffer('0001', 'hex')); // distance: 0000000000000010
    test.deepEqual(contacts[1].id, new Buffer('0103', 'hex')); // distance: 0000000100000000
    test.deepEqual(contacts[2].id, new Buffer('0102', 'hex')); // distance: 0000000100000010
    test.deepEqual(contacts[3].id, new Buffer('0101', 'hex'));
    test.deepEqual(contacts[4].id, new Buffer('0100', 'hex'));
    test.deepEqual(contacts[5].id, new Buffer('0107', 'hex'));
    test.deepEqual(contacts[6].id, new Buffer('0106', 'hex'));
    test.deepEqual(contacts[7].id, new Buffer('0105', 'hex'));
    test.deepEqual(contacts[8].id, new Buffer('0104', 'hex'));
    test.deepEqual(contacts[9].id, new Buffer('010b', 'hex'));
    test.deepEqual(contacts[10].id, new Buffer('010a', 'hex'));
    test.deepEqual(contacts[11].id, new Buffer('0109', 'hex'));
    test.deepEqual(contacts[12].id, new Buffer('0108', 'hex'));
    test.deepEqual(contacts[13].id, new Buffer('010f', 'hex'));
    test.deepEqual(contacts[14].id, new Buffer('010e', 'hex'));
    test.deepEqual(contacts[15].id, new Buffer('010d', 'hex'));
    test.deepEqual(contacts[16].id, new Buffer('010c', 'hex'));
    test.deepEqual(contacts[17].id, new Buffer('0113', 'hex'));
    test.deepEqual(contacts[18].id, new Buffer('0112', 'hex'));
    test.deepEqual(contacts[19].id, new Buffer('0111', 'hex'));
    test.deepEqual(contacts[20].id, new Buffer('0110', 'hex'));
    test.deepEqual(contacts[21].id, new Buffer('8003', 'hex')); // distance: 1000000000000000
    // console.log(require('util').inspect(kBucket, false, null));
    test.done();
  };
*)
  ]
