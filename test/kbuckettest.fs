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
  ]
