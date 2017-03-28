module KBucketTest

open MochaTest
open Buffer
open KBucket

let (=>) (a : string) (b : 'b) : (string * 'b) = (a,b)

let nodeId (b : string) : Buffer =
  let hasher = Crypto.createHash "sha1" in
  let bBuf = Buffer.fromString b "utf-8" in
  let _ = Crypto.updateBuffer bBuf hasher in
  let digest = Crypto.digestBuffer hasher in
  Buffer.slice 0 20 digest

type KBucketNode = { id : Buffer ; vectorClock : int ; foo : string }

let newContactString (b : string) : KBucketNode =
  { id = nodeId b ; vectorClock = 0 ; foo = "" }

let newContactBuffer (b : Buffer) : KBucketNode =
  { id = b ; vectorClock = 0 ; foo = "" }

let kbOps : KBucketAbstract<Buffer,KBucketNode> =
  { distance = KBucket.defaultDistance
  ; nodeId = fun a -> a.id
  ; arbiter =
      fun (a : KBucketNode) (b : KBucketNode) ->
        if a.vectorClock > b.vectorClock then a else b
  ; keyLength = Buffer.length
  ; keyNth = Buffer.at
  ; idEqual = Buffer.equal
  ; idLess = fun a b -> Buffer.compare a b < 0
  }

let kbadd
      (kb : KBucket<Buffer,KBucketNode> ref )
      (pings : Action<Buffer,KBucketNode> list ref)
      (contact : KBucketNode) : unit =
  let (kbu,pu) = KBucket.add kbOps !kb contact None in
  begin
    kb := kbu ;
    pings := !pings @ pu
  end

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
        let pings = ref [] in
        for j = 0 to KBucket.Constants.DEFAULT_NUMBER_OF_NODES_PER_K_BUCKET do
          begin
            let iString = [| 0x80; j |] in
            kbadd kb pings (newContactBuffer (Buffer.fromArray iString)) ;
          end ;
        let _ =
          match !pings with
          | [Ping (contacts, replacement)] ->
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
            kbadd kb pings (newContactBuffer (Buffer.fromArray [|i|]))
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
            kbadd kb pings (newContactBuffer (Buffer.fromArray [|i|]))
          end ;
        let contact = (newContactBuffer (Buffer.fromArray [|0x11|])) in
        let contacts = KBucket.closest kbOps !kb contact 3 None in
        massert.ok (contacts.[0].id = (Buffer.fromArray [|0x11|])) ;
        massert.ok (contacts.[1].id = (Buffer.fromArray [|0x10|])) ;
        massert.ok (contacts.[2].id = (Buffer.fromArray [|0x01|])) ;
        donef ()

  ; "closest nodes are returned even if there isn\'t enough in one bucket" =>
      fun donef ->
        let kb = ref (KBucket.init (Buffer.fromArray [|0x7f;0|])) in
        let pings = ref [] in
        for i = 0 to KBucket.Constants.DEFAULT_NUMBER_OF_NODES_PER_K_BUCKET - 1 do
          begin
            let farString = Buffer.fromArray [|0x80;i|] in
            let nearString = Buffer.fromArray [|0x01;i|] in
            kbadd kb pings (newContactBuffer farString) ;
            kbadd kb pings (newContactBuffer nearString)
          end ;
        kbadd kb pings (newContactBuffer (Buffer.fromArray [|0;1|])) ;
        let contact = newContactBuffer (Buffer.fromArray [|0;3|]) in
        let contacts = KBucket.closest kbOps !kb contact 22 None in
        let expectedList =
          [| [|0;1|]
          ; [|1;3|]
          ; [|1;2|]
          ; [|1;1|]
          ; [|1;0|]
          ; [|1;7|]
          ; [|1;6|]
          ; [|1;5|]
          ; [|1;4|]
          ; [|1;0xb|]
          ; [|1;0xa|]
          ; [|1;9|]
          ; [|1;8|]
          ; [|1;0xf|]
          ; [|1;0xe|]
          ; [|1;0xd|]
          ; [|1;0xc|]
          ; [|1;0x13|]
          ; [|1;0x12|]
          ; [|1;0x11|]
          ; [|1;0x10|]
          ; [|0x80;3|]
          |] |> Array.map Buffer.fromArray
        in
        let actualList = contacts |> Array.map (fun n -> n.id) in
        massert.ok (expectedList = actualList) ;
        donef ()

  ; "count returns 0 when no contacts in bucket" =>
      fun donef ->
        let kb = KBucket.init (nodeId (ShortId.generate ())) in
        massert.ok (KBucket.count kb = 0) ;
        donef ()

  ; "count returns 1 when 1 contact in bucket" =>
      fun donef ->
        let kb = ref (KBucket.init (nodeId (ShortId.generate ()))) in
        let pings = ref [] in
        let contact = newContactString "a" in
        kbadd kb pings contact ;
        massert.ok (KBucket.count !kb = 1) ;
        donef ()

  ; "count returns 1 when same contact added to bucket twice" =>
      fun donef ->
        let kb = ref (KBucket.init (nodeId (ShortId.generate ()))) in
        let pings = ref [] in
        let contact = newContactString "a" in
        kbadd kb pings contact ;
        kbadd kb pings contact ;
        massert.ok (KBucket.count !kb = 1) ;
        donef ()

  ; "count returns number of added unique contacts" =>
      fun donef ->
        let kb = ref (KBucket.init (nodeId (ShortId.generate ()))) in
        let pings = ref [] in
        let _ =
          Seq.fold
            (fun _ str ->
              kbadd kb pings (newContactString str)
            )
            ()
            [ "a" ; "a" ; "b" ; "b" ; "c" ; "d" ; "c" ; "d" ; "e" ; "f" ]
        in
        massert.ok (KBucket.count !kb = 6) ;
        donef ()

  ; "localNodeId should be a random SHA-1 if not provided" =>
      fun donef ->
        let kb = KBucket.init (nodeId (ShortId.generate ())) in
        massert.ok (Buffer.length kb.localNodeId = 20) ;
        donef ()

  ; "localNodeId is a Buffer populated from options if options.localNodeId Buffer is provided" =>
      fun donef ->
        let localNodeId = Buffer.fromString "some length" "utf-8" in
        let kb = KBucket.init localNodeId in
        massert.ok (kb.localNodeId = localNodeId) ;
        donef ()

  ; "id 00000000, bitIndex 0, should be low" =>
      fun donef ->
        let kb = KBucket.init (nodeId (ShortId.generate ())) in
        massert.ok
          ((KBucket.determineBucket kbOps kb (Buffer.fromArray [|0|]) (Some 0)) =
             -1) ;
        donef ()

  ; "id 01000000, bitIndex 0, should be low" =>
      fun donef ->
        let kb = KBucket.init (nodeId (ShortId.generate ())) in
        massert.ok
          ((KBucket.determineBucket kbOps kb (Buffer.fromArray [|0x40|]) (Some 0)) =
             -1) ;
        donef ()

  ; "id 01000000, bitIndex 1, should be high" =>
      fun donef ->
        let kb = KBucket.init (nodeId (ShortId.generate ())) in
        massert.ok
          ((KBucket.determineBucket kbOps kb (Buffer.fromArray [|0x40|]) (Some 1)) =
             1) ;
        donef ()

  ; "id 01000000, bitIndex 2, should be low" =>
      fun donef ->
        let kb = KBucket.init (nodeId (ShortId.generate ())) in
        massert.ok
          ((KBucket.determineBucket kbOps kb (Buffer.fromArray [|0x40|]) (Some 2)) =
             -1) ;
        donef ()

  ; "id 01000000, bitIndex 9, should be low" =>
      fun donef ->
        let kb = KBucket.init (nodeId (ShortId.generate ())) in
        massert.ok
          ((KBucket.determineBucket kbOps kb (Buffer.fromArray [|0x40|]) (Some 9)) =
             -1) ;
        donef ()

  ; "id 01000001, bitIndex 7, should be high" =>
      fun donef ->
        let kb = KBucket.init (nodeId (ShortId.generate ())) in
        massert.ok
          ((KBucket.determineBucket kbOps kb (Buffer.fromArray [|0x41|]) (Some 7)) =
             1) ;
        donef ()

  ; "id 0100000100000000, bitIndex 7, should be high" =>
      fun donef ->
        let kb = KBucket.init (nodeId (ShortId.generate ())) in
        massert.ok
          ((KBucket.determineBucket kbOps kb (Buffer.fromArray [|0x41;0|]) (Some 7)) =
             1) ;
        donef ()

  ; "id 000000000100000100000000, bitIndex 15, should be high" =>
      fun donef ->
        let kb = KBucket.init (nodeId (ShortId.generate ())) in
        massert.ok
          ((KBucket.determineBucket kbOps kb (Buffer.fromArray [|0;41;0|]) (Some 15)) =
             1) ;
        donef ()

  ; "get retrieves null if no contacts" =>
      fun donef ->
        let kb = KBucket.init (nodeId (ShortId.generate ())) in
        massert.ok
          ((KBucket.get kbOps kb (nodeId "foo") None) = None) ;
        donef ()

  ; "get retrieves a contact that was added" =>
      fun donef ->
        let kb = ref (KBucket.init (nodeId (ShortId.generate ()))) in
        let pings = ref [] in
        let id = Buffer.fromString "a" "utf-8" in
        let contact = newContactBuffer id in
        kbadd kb pings contact ;
        massert.ok
          ((KBucket.get kbOps !kb id None) = Some (newContactBuffer id)) ;
        donef ()

  ; "get retrieves most recently added contact if same id" =>
      fun donef ->
        let kb = ref (KBucket.init (nodeId (ShortId.generate ()))) in
        let pings = ref [] in
        let contact =
          { newContactString "a" with
              foo = "foo" ;
              vectorClock = 0
          }
        in
        let contact2 =
          { newContactString "a" with
              foo = "bar" ;
              vectorClock = 1
          }
        in
        kbadd kb pings contact ;
        kbadd kb pings contact2 ;
        massert.ok ((KBucket.get kbOps !kb (nodeId "a") None) = Some contact2) ;
        donef ()

  ; "get retrieves contact from nested leaf node" =>
      fun donef ->
        let kb = ref (KBucket.init (Buffer.fromArray [|0;0|])) in
        let pings = ref [] in
        for i = 0 to KBucket.Constants.DEFAULT_NUMBER_OF_NODES_PER_K_BUCKET - 1 do
          begin
            let iString = Buffer.fromArray [|0x80;i|] in
            kbadd kb pings (newContactBuffer iString)
          end ;
        let finalContactId = 
          Buffer.fromArray
            [|0;0x80;
              KBucket.Constants.DEFAULT_NUMBER_OF_NODES_PER_K_BUCKET-1
            |]
        in
        kbadd kb pings { newContactBuffer finalContactId with foo = "me" } ;
        massert.ok
          (((KBucket.get kbOps !kb finalContactId None)
            |> KBucket.optionMap (fun ctc -> ctc.foo)) = Some "me") ;
        donef ()

  ; "indexOf returns a contact with id that contains the same byte sequence as the test contact" =>
      fun donef ->
        let kb = ref (KBucket.init (nodeId (ShortId.generate ()))) in
        let pings = ref [] in
        let id = nodeId "a" in
        kbadd kb pings (newContactBuffer id) ;
        massert.ok
          ((KBucket.indexOf kbOps !kb (newContactBuffer id)) = 0) ;
        donef ()

  ; "indexOf returns -1 if contact is not found" =>
      fun donef ->
        let kb = ref (KBucket.init (nodeId (ShortId.generate ()))) in
        let pings = ref [] in
        let id = nodeId "a" in
        kbadd kb pings (newContactBuffer id) ;
        massert.ok
          ((KBucket.indexOf kbOps !kb (newContactBuffer (nodeId "b"))) = -1) ;
        donef ()

  ; "removing a contact should remove contact from nested buckets" =>
      fun donef ->
        let kb = ref (KBucket.init (Buffer.fromArray [|0;0|])) in
        let pings = ref [] in
        for i = 0 to KBucket.Constants.DEFAULT_NUMBER_OF_NODES_PER_K_BUCKET - 1 do
          begin
            let iString = Buffer.fromArray [|0x80;i|] in
            kbadd kb pings (newContactBuffer iString)
          end ;
        let _ =
          kbadd kb pings
            (newContactBuffer
               (Buffer.fromArray
                  [|0;KBucket.Constants.DEFAULT_NUMBER_OF_NODES_PER_K_BUCKET-1|]
               )
            ) ;
        let toDelete = (newContactBuffer (Buffer.fromArray [|0x80;0|])) in
        let highBucketMatch toDelete =
          match (!kb).high with
          | Some high ->
             match high.bucket with
             | Some bucket ->
                let firstMatch =
                  bucket |> Seq.mapi (fun i c -> (i,c))
                  |> Seq.skipWhile (fun (i,c) -> c.id <> toDelete.id)
                  |> Seq.truncate 1
                  |> List.ofSeq
                in
                firstMatch <> []
             | None -> false
           | None -> false
        in
        let _ = massert.ok (highBucketMatch toDelete) in
        let (kb2,_) = KBucket.remove kbOps !kb toDelete None in
        let _ = kb := kb2 in
        massert.ok (not (highBucketMatch toDelete)) ;
        donef ()

  ; "adding a contact does not split K-bucket" =>
      fun donef ->
        let kb = ref (KBucket.init (nodeId (ShortId.generate ()))) in
        let pings = ref [] in
        kbadd kb pings (newContactString "a") ;
        (match ((!kb).bucket,(!kb).low,(!kb).high) with
         | (Some bucket,None,None) -> massert.ok true
         | _ -> massert.ok false
        ) ;
        donef ()

  ; (String.concat " "
       ["adding maximum number of contacts (per K-bucket) [" ;
        string KBucket.Constants.DEFAULT_NUMBER_OF_NODES_PER_K_BUCKET ;
        "]" ;
        " into K-bucket does not split K-bucket"
       ]
    ) =>
      fun donef ->
        let kb = ref (KBucket.init (nodeId (ShortId.generate ()))) in
        let pings = ref [] in
        for i = 0 to KBucket.Constants.DEFAULT_NUMBER_OF_NODES_PER_K_BUCKET - 1 do
          begin
            kbadd kb pings (newContactString (string i))
          end ;
        (match ((!kb).bucket,(!kb).low,(!kb).high) with
         | (Some bucket,None,None) -> massert.ok true
         | _ -> massert.ok false
        ) ;
        donef ()

  ; (String.concat " "
       ["adding maximum number of contacts (per K-bucket) + 1 [" ;
        string (KBucket.Constants.DEFAULT_NUMBER_OF_NODES_PER_K_BUCKET + 1) ;
        "]" ;
        " into K-bucket splits the K-bucket"
       ]
    ) =>
      fun donef ->
        let kb = ref (KBucket.init (nodeId (ShortId.generate ()))) in
        let pings = ref [] in
        for i = 0 to KBucket.Constants.DEFAULT_NUMBER_OF_NODES_PER_K_BUCKET do
          begin
            kbadd kb pings (newContactString (string i))
          end ;
        (match ((!kb).bucket,(!kb).low,(!kb).high) with
         | (None,Some low,Some high) -> massert.ok true
         | _ -> massert.ok false
        ) ;
        donef ()

(*
    var traverse = function (node) {
        if (!node.bucket) {
            traverse(node.low);
            traverse(node.high);
             } else {
            node.bucket.forEach(function (contact) {
                foundContact[contact.id.toString('hex')] = true;
                                           });
          }
                              };
    traverse(kBucket);
 *)

  ; "split buckets contain all added contacts" =>
      fun donef ->
        let rec traverse kb =
          match (kb.bucket,kb.low,kb.high) with
          | (Some bucket,None,None) ->
             Seq.concat [bucket]
          | (None,Some low,Some high) ->
             Seq.concat [traverse low;traverse high]
          | _ -> failwith "Expected bucket or low,high"
        in
        let kb = ref (KBucket.init (Buffer.fromArray [|0|])) in
        let pings = ref [] in
        let foundContact = ref Map.empty in
        for i = 0 to KBucket.Constants.DEFAULT_NUMBER_OF_NODES_PER_K_BUCKET + 1 do
          begin
            let iString = Buffer.fromArray [|i|] in
            kbadd kb pings (newContactBuffer iString) ;
            foundContact := Map.add i false !foundContact
          end ;
        let _ =
          traverse !kb
          |> Seq.iter
               (fun elt ->
                 let i = Buffer.at 0 elt.id in
                 foundContact := Map.add i true !foundContact
               )
        in
        massert.ok
          (!foundContact
           |> Map.toSeq
           |> Seq.exists (fun (k,v) -> not v)
           |> not
          ) ;
        massert.ok ((!kb).bucket = None) ;
        donef ()

  ; "when splitting buckets the \"far away\" bucket should be marked to prevent splitting \"far away\" bucket" =>
      fun donef ->
        let rec traverse node dontSplit =
          match (node.bucket,node.low,node.high) with
          | (None, Some low, Some high) ->
             traverse low false ;
             traverse high true
          | (Some bucket, _, _) ->
             massert.ok (node.dontSplit = dontSplit)
          | _ -> failwith "Expected bucket or low,high"
        in
        let kb = ref (KBucket.init (Buffer.fromArray [|0|])) in
        let pings = ref [] in
        for i = 0 to KBucket.Constants.DEFAULT_NUMBER_OF_NODES_PER_K_BUCKET do
          begin
            let iString = Buffer.fromArray [|i|] in
            kbadd kb pings (newContactBuffer iString)
          end ;
        (* above algorithm will split low bucket 4 times and put 0x00 through 0x0f
        // in the low bucket, and put 0x10 through 0x14 in high bucket
        // since localNodeId is 0x00, we expect every high bucket to be "far" and
        // therefore marked as "dontSplit = true"
        // there will be one "low" bucket and four "high" buckets (test.expect(5))
        *)
        traverse !kb false ;
        donef ()

  ; "toArray should return empty array if no contacts" =>
      fun donef ->
        let kb = KBucket.init (nodeId (ShortId.generate ())) in
        massert.ok ((Array.length (KBucket.toArray kb)) = 0) ;
        donef ()

  ; "toArray should return all contacts in an array arranged from low to high buckets" =>
      fun donef ->
        let expectedIds = ref [] in
        let kb = ref (KBucket.init (Buffer.fromArray [|0;0|])) in
        let pings = ref [] in
        for i = 0 to KBucket.Constants.DEFAULT_NUMBER_OF_NODES_PER_K_BUCKET - 1 do
          begin
            let iString = Buffer.fromArray [|0x80;i|] in
            expectedIds := iString :: !expectedIds ;
            kbadd kb pings (newContactBuffer iString)
          end ;
        (* cause a split to happen *)
        kbadd kb pings
          (newContactBuffer
             (Buffer.fromArray
                [|0;KBucket.Constants.DEFAULT_NUMBER_OF_NODES_PER_K_BUCKET-1|]
             )
          ) ;
        let contacts = KBucket.toArray !kb in
        massert.ok 
          (Array.length contacts =
             KBucket.Constants.DEFAULT_NUMBER_OF_NODES_PER_K_BUCKET + 1) ;
        massert.ok
          (contacts.[0].id = 
             Buffer.fromArray
               [|0x80;KBucket.Constants.DEFAULT_NUMBER_OF_NODES_PER_K_BUCKET-1|]
          ) ;
        let contacts2 = contacts |> Seq.skip 1 |> Array.ofSeq in
        let expectedArray = List.rev !expectedIds |> Array.ofSeq in
        for i = 0 to KBucket.Constants.DEFAULT_NUMBER_OF_NODES_PER_K_BUCKET - 1 do
          begin
            massert.ok (contacts2.[i].id = expectedArray.[i])
          end ;
        donef ()
  ]
