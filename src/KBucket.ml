(* Immutable Kademlia K-Bucket implementation based on
 * https://github.com/vojnovski/k-bucket

Copyright (c) 2013-2014 Tristan Slominski

Permission is hereby granted, free of charge, to any person
obtaining a copy of this software and associated documentation
files (the "Software"), to deal in the Software without
restriction, including without limitation the rights to use,
copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the
Software is furnished to do so, subject to the following
conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.                                                  
THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.
 *)

open Util

let _DEFAULT_NUMBER_OF_NODES_PER_K_BUCKET = 20
let _DEFAULT_NUMBER_OF_NODES_TO_PING = 3

type ('id,'a) storage =
  | Self of ('a array)
  | Split of (('id,'a) kbucket * ('id,'a) kbucket)
                                          
and ('id,'a) kbucket =
  { localNodeId : 'id
  ; dontSplit : bool
  ; storage : ('id,'a) storage
  ; ver : int
  }

type ('id,'a) kbucketAbstract =
  { distance : ('id,'a) kbucketAbstract -> 'id -> 'id -> int array
  ; nodeId : 'a -> 'id
  ; arbiter : 'a -> 'a -> 'a
  ; keyLength : 'id -> int
  ; keyNth : int -> 'id -> int
  ; idEqual : 'id -> 'id -> bool
  ; idLess : 'id -> 'id -> bool
  }

type ('id,'a) action =
  | Ping of ('a array * 'a)
  
let optionDefault (d : 'a) (o : 'a option) =
  match o with
  | None -> d
  | Some v -> v

let arrayRemove (at : int) (n : int) (a : 'a array) : 'a array =
  Array.to_list a
  |> List.mapi (fun i e -> (i,e))
  |> List.filter (fun (i,e) -> i < at || i >= at + n)
  |> List.map (fun (i,e) -> e)
  |> Array.of_list

let listHeadOption l =
  match l with
  | hd :: tl -> Some hd
  | _ -> None

let (&>) (self : 'v) (todo : (('id,'a) action) list) : ('v * (('id,'a) action) list) =
  (self, todo)

let (&+) ((s,a) : ('v * (('id,'a) action) list)) (f : 'v -> ('v * (('id,'a) action) list)) : ('v * (('id,'a) action) list) =
  let (ns,na) = f s in
  (ns,a @ na)

let init localNodeId =
  { localNodeId = localNodeId
  ; dontSplit = false
  ; storage = Self [||]
  ; ver = 0
  }

(* Returns the index of the contact if it exists *)
let indexOf ops (id : string) self =
  match self.storage with
  | Self bucket ->
     let idxOpt =
       bucket
       |> Array.to_list
       |> List.mapi (fun i c -> (i,c))
       |> Util.skipWhile (fun (i,c) -> not (ops.idEqual id (ops.nodeId c)))
       |> Util.truncate 1
       |> List.map (fun (i,c) -> i)
       |> listHeadOption
     in
     (match idxOpt with
      | Some i -> i
      | None -> -1
     )
  | _ -> -1

(* We can compare arrays of ints so we'll express the distance that way for lack of anything
 * better for now. *)
let defaultDistance ops firstId secondId =
  let fl = ops.keyLength firstId in
  let sl = ops.keyLength secondId in
  let (longer,shorter) = if fl > sl then (firstId,secondId) else (secondId,firstId) in
  let ll = ops.keyLength longer in
  let sl = ops.keyLength shorter in
  let accumulator = Bytes.make ll (Char.chr 0) in
  for i = 0 to (ll - 1) do
    begin
      if i >= sl then
        accumulator.[i] <- (Char.chr 255)
      else
        accumulator.[i] <- (Char.chr ((ops.keyNth i firstId) lxor (ops.keyNth i secondId)))
    end
  done ;
  accumulator

(* Determines whether the id at the bitIndex is 0 or 1. If 0, returns -1, else 1
// id: a Buffer to compare localNodeId with
// bitIndex: the bitIndex to which bit to check in the id Buffer
*)
let determineBucket ops id bitIndexOpt =
  let bitIndex = bitIndexOpt |> optionDefault 0 in

  (* **NOTE** remember that id is a Buffer and has granularity of
  // bytes (8 bits), whereas the bitIndex is the _bit_ index (not byte)
  // id's that are too short are put in low bucket (1 byte = 8 bits)
  // parseInt(bitIndex / 8) finds how many bytes the bitIndex describes
  // bitIndex % 8 checks if we have extra bits beyond byte multiples
  // if number of bytes is <= no. of bytes described by bitIndex and there
  // are extra bits to consider, this means id has less bits than what
  // bitIndex describes, id therefore is too short, and will be put in low
  // bucket        
  *)

  let bytesDescribedByBitIndex = bitIndex / 8 in
  let bitIndexWithinByte = bitIndex mod 8 in
  
  if ops.keyLength id <= bytesDescribedByBitIndex && bitIndexWithinByte <> 0 then
    -1
  else
    let byteUnderConsideration = ops.keyNth bytesDescribedByBitIndex id in

    (* byteUnderConsideration is an integer from 0 to 255 represented by 8 bits
    // where 255 is 11111111 and 0 is 00000000
    // in order to find out whether the bit at bitIndexWithinByte is set
    // we construct Math.pow(2, (7 - bitIndexWithinByte)) which will consist
    // of all bits being 0, with only one bit set to 1
    // for example, if bitIndexWithinByte is 3, we will construct 00010000 by
    // Math.pow(2, (7 - 3)) -> Math.pow(2, 4) -> 16
    *)
    let bit = (1 lsl (7 - bitIndexWithinByte)) in
    let bitValue = (byteUnderConsideration land bit) <> 0 in
    
    if bitValue then
      1
    else
      -1
      
type ('id,'a) internalOps =
  { update : ('id,'a) kbucketAbstract -> ('id,'a) kbucket -> 'a -> int -> ((('id,'a) kbucket) * (('id,'a) action) list) ;
    splitAndAddInternal : (('id,'a) kbucketAbstract) -> ('id,'a) internalOps -> ('id,'a) kbucket -> 'a -> int option -> (('id,'a) kbucket * (('id,'a) action) list) ;
    addInternal : (('id,'a) kbucketAbstract) -> ('id,'a) internalOps -> ('id,'a) kbucket -> 'a -> int option -> (('id,'a) kbucket * (('id,'a) action) list)
  }

let rec addInternal
      (ops : (('id,'a) kbucketAbstract))
      (intr : ('id,'a) internalOps)
      (self : ('id,'a) kbucket)
      (contact : 'a)
      (bitIndexOpt : int option) : (('id,'a) kbucket * ('id,'a) action list) =
  let bitIndex = bitIndexOpt |> optionDefault 0 in
  let bitIndexPP = bitIndex + 1 in
  match self.storage with
  | Split (low,high) ->
      if (determineBucket ops (ops.nodeId contact) (Some (bitIndex))) < 0 then
        (* this is not a leaf node but an inner node with 'low' and 'high'
        // branches; we will check the appropriate bit of the identifier and
        // delegate to the appropriate node for further processing
        *)
        intr.addInternal ops intr low contact (Some bitIndexPP) &+
          (fun low -> { self with storage = Split (low,high) ; ver = self.ver + 1 } &> [])
      else
        intr.addInternal ops intr high contact (Some bitIndexPP) &+
          (fun high -> { self with storage = Split (low,high) ; ver = self.ver + 1 } &> [])
  | Self bucket ->
     (* Check if the contact already exists *)
     let index = indexOf ops (ops.nodeId contact) self in
     if index >= 0 then
       intr.update ops self contact index
     else
       (* The bucket is full *)
       if Array.length bucket < _DEFAULT_NUMBER_OF_NODES_PER_K_BUCKET then
         { self with storage = Self (Array.append bucket [|contact|]) ; ver = self.ver + 1 } &> []
       else if self.dontSplit then
         (* we are not allowed to split the bucket
         // we need to ping the first constants.DEFAULT_NUMBER_OF_NODES_TO_PING
         // in order to determine if they are alive
         // only if one of the pinged nodes does not respond, can the new contact
         // be added (this prevents DoS flodding with new invalid contacts)
         *)
         self &>
           [Ping (Array.sub bucket 0 _DEFAULT_NUMBER_OF_NODES_TO_PING, contact)]
       else
         intr.splitAndAddInternal ops intr self contact (Some bitIndex)

(* contact: Object *required* contact object
// id: Buffer *require* node id
// n: Integer *required* maximum number of closest contacts to return
// bitIndex: Integer (Default: 0)
// Return: Array of maximum of `n` closest contacts to the `contact`
*)
let rec closest
      (ops : (('id,'a) kbucketAbstract))
      (self : ('id,'a) kbucket)
      (contact : 'id)
      (n : int)
      (bitIndexOpt : int option) : 'a array =
  let bitIndex = bitIndexOpt |> optionDefault 0 in
  let contacts =
    match self.storage with
    | Split (low,high) ->
       if determineBucket ops contact (Some bitIndex) < 0 then
         begin
           let contactsVal = closest ops low contact n (Some (bitIndex + 1)) in
           if Array.length contactsVal < n then
             let contactsVal2 = closest ops high contact n (Some (bitIndex + 1)) in
             Array.append contactsVal contactsVal2
           else
             contactsVal
         end
       else
         begin
           let contactsVal = closest ops high contact n (Some (bitIndex + 1)) in
           if Array.length contactsVal < n then
             let contactsVal2 = closest ops low contact n (Some (bitIndex + 1)) in
             Array.append contactsVal contactsVal2
           else
             contactsVal
         end
    | Self bucket -> bucket
  in
  let distances =
    Array.map
      (fun storedContact ->
        (storedContact, ops.distance ops (ops.nodeId storedContact) contact)
      )
      contacts
  in
  let sorted =
    Util.arraySortWith
      (fun (a,ad) (b,bd) ->
        if ad < bd then
          -1
        else if ad > bd then
          1
        else
          0
      )
      distances
  in
  let (sortedContacts : 'a array) =
    Array.map (fun (a,ad) -> a) sorted
  in
  (Array.sub sortedContacts 0 n)
                
(* Counts the number of contacts recursively.
// If this is a leaf, just return the number of contacts contained. Otherwise,
// return the length of the high and low branches combined.
*)
let rec count (self : ('id,'a) kbucket) =
  match self.storage with
  | Self bucket ->
    Array.length bucket
  | Split (low,high) ->
    (count high) + (count low)

(* Get a contact by its exact ID.
// If this is a leaf, loop through the bucket contents and return the correct
// contact if we have it or null if not. If this is an inner node, determine
// which branch of the tree to traverse and repeat.
// id: *required* a Buffer specifying the ID of the contact to fetch
// bitIndex: the bitIndex to which bit to check in the Buffer for navigating
//           the binary tree
*)
let rec get (ops : ('id,'a) kbucketAbstract) (self : ('id,'a) kbucket) (id : 'id) (bitIndexOpt : int option) : 'a option =
  let bitIndex = bitIndexOpt |> optionDefault 0 in
  let bitIndexPP = bitIndex + 1 in

  match self.storage with
  | Split (low,high) ->
      if determineBucket ops id (Some bitIndex) < 0 then
        get ops low id (Some bitIndexPP)
      else
        get ops high id (Some bitIndexPP)
  | Self bucket ->
     let index = indexOf ops id self in (* index of uses contact.id for matching *)

     if index < 0 then
       None (* contact not found *)
     else
       Some (Array.get bucket index)

(* contact: *required* the contact object to remove
// bitIndex: the bitIndex to which bit to check in the Buffer for navigating
//           the binary tree
*)
let rec remove (ops : (('id,'a) kbucketAbstract)) (self : ('id,'a) kbucket) (contact : 'id) (bitIndexOpt : int option) =
  (* first check whether we are an inner node or a leaf (with bucket contents) *)
  match self.storage with
  | Split (low,high) ->
     (* this is not a leaf node but an inner node with 'low' and 'high'
     // branches; we will check the appropriate bit of the identifier and
     // delegate to the appropriate node for further processing
     *)
     let bitIndex = bitIndexOpt |> optionDefault 0 in
     let bitIndexPP = bitIndex + 1 in

     if determineBucket ops contact (Some bitIndex) < 0 then
       remove ops low contact (Some bitIndexPP)
     else
       remove ops high contact (Some bitIndexPP)
  | Self bucket ->
     let index = indexOf ops contact self in
     if index >= 0 then
       { self with storage = Self (arrayRemove index 1 bucket) } &> []
     else
       self &> []

(* Splits the bucket, redistributes contacts to the new buckets, and marks the
// bucket that was split as an inner node of the binary tree of buckets by
// setting self.bucket = undefined;
// contact: *required* the contact object to add
// bitIndex: the bitIndex to which byte to check in the Buffer for navigating the
//          binary tree
*)
let rec splitAndAddInternal
    (ops : (('id,'a) kbucketAbstract))
    (intr : ('id,'a) internalOps)
    (self : ('id,'a) kbucket)
    (contact : 'a)
    (bitIndexOpt : int option) =
  let bitIndex = bitIndexOpt |> optionDefault 0 in
  match self.storage with
  | Self bucket ->
     let newLowBucket =
       Array.to_list bucket
       |> List.filter
         (fun storedContact ->
           determineBucket ops (ops.nodeId storedContact) (Some bitIndex) < 0
         )
       |> Array.of_list
     in
     let newHighBucket =
       Array.to_list bucket
       |> List.filter
         (fun storedContact ->
           determineBucket ops (ops.nodeId storedContact) (Some bitIndex) >= 0
         )
       |> Array.of_list
     in
     (* don't split the "far away" bucket
     // we check where the local node would end up and mark the other one as
     // "dontSplit" (i.e. "far away")
     *)
     let whichBucketIsFar =
       determineBucket ops self.localNodeId (Some bitIndex) < 0
     in
     let low =
       { (init self.localNodeId) with
         storage = Self newLowBucket ;
         dontSplit = not whichBucketIsFar
       }
     in
     let high =
       { (init self.localNodeId) with
         storage = Self newHighBucket ;
         dontSplit = whichBucketIsFar
       }
     in
     let splitSelf =
       { self with storage = Split (low,high) }
     in
     (* add the contact being added *)
     addInternal ops intr splitSelf contact (Some bitIndex)
  | _ -> failwith "Expected bucket"
      

(* Returns all the contacts contained in the tree as an array.
// If self is a leaf, return a copy of the bucket. `slice` is used so that we
// don't accidentally leak an internal reference out that might be accidentally
// misused. If self is not a leaf, return the union of the low and high
// branches (themselves also as arrays).
 *)
let rec toArray (self : ('id,'a) kbucket) : 'a array =
  match self.storage with
  | Self bucket -> bucket
  | Split (low,high) -> Array.append (toArray low) (toArray high)

(* Updates the contact selected by the arbiter.
// If the selection is our old contact and the candidate is some new contact
// then the new contact is abandoned (not added).
// If the selection is our old contact and the candidate is our old contact
// then we are refreshing the contact and it is marked as most recently
// contacted (by being moved to the right/end of the bucket array).
// If the selection is our new contact, the old contact is removed and the new
// contact is marked as most recently contacted.
// contact: *required* the contact to update
// index: *required* the index in the bucket where contact exists
//        (index has already been computed in a previous calculation)
*)
let update (ops : ('id,'a) kbucketAbstract) (self : ('id,'a) kbucket) (contact : 'a) (index : int) =
  match self.storage with
  | Self bucket ->
     begin
       let _ =
         if not (ops.idEqual (ops.nodeId (Array.get bucket index)) (ops.nodeId contact)) then
           failwith "indexOf() calculation resulted in wrong index"
       in
       let (incumbent : 'a) = Array.get bucket index in
       let selection = ops.arbiter incumbent contact in
       if selection = incumbent && incumbent <> contact then
         (* if the selection is our old contact and the candidate is some new
         // contact, then there is nothing to do
         *)
         self &> []
       else
         { self with
             storage =
               Self
                 (Array.append (arrayRemove index 1 bucket) [|selection|]) ;
             ver = self.ver + 1
         } &> []
     end
  | _ -> failwith "Expected bucket"

let internalOps_ =
  { update = update
  ; addInternal = addInternal
  ; splitAndAddInternal = splitAndAddInternal
  }

(* contact: *required* the contact object to add
// bitIndex: the bitIndex to which bit to check in the Buffer for navigating
//           the binary tree
*)
let add
      (ops : (('id,'a) kbucketAbstract))
      (self : ('id,'a) kbucket)
      (contact : 'a)
      (bitIndexOpt : int option) : (('id,'a) kbucket * ('id,'a) action list) =
  addInternal ops internalOps_ self contact bitIndexOpt

let splitAndAdd
    (ops : (('id,'a) kbucketAbstract))
    (self : ('id,'a) kbucket)
    (contact : 'a)
    (bitIndexOpt : int option) =
  splitAndAddInternal ops internalOps_  self contact bitIndexOpt
