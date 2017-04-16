module KBucket

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

module Constants =
  let DEFAULT_NUMBER_OF_NODES_PER_K_BUCKET = 20
  let DEFAULT_NUMBER_OF_NODES_TO_PING = 3

type KBucket<'id,'a> =
  { localNodeId : 'id
  ; bucket : 'a array option
  ; dontSplit : bool
  ; low : KBucket<'id,'a> option
  ; high : KBucket<'id,'a> option
  }

type KBucketAbstract<'id,'a> =
  { distance : KBucketAbstract<'id,'a> -> 'id -> 'id -> int array
  ; nodeId : 'a -> 'id
  ; arbiter : 'a -> 'a -> 'a
  ; keyLength : 'id -> int
  ; keyNth : int -> 'id -> int
  ; idEqual : 'id -> 'id -> bool
  ; idLess : 'id -> 'id -> bool
  }

type Action<'id,'a> =
  | Ping of ('a array * 'a)

let optionMap (f : 'a -> 'b) (o : 'a option) =
  match o with
  | None -> None
  | Some v -> Some (f v)

let optionThen (f : 'a -> 'b option) (o : 'a option) =
  match o with
  | None -> None
  | Some v -> f v

let optionDefault (d : 'a) (o : 'a option) =
  match o with
  | None -> d
  | Some v -> v

let optionDefLazy (f : unit -> 'a) (o : 'a option) =
  match o with
  | None -> f ()
  | Some v -> v

let arrayRemove (at : int) (n : int) (a : 'a array) : 'a array =
  Seq.mapi (fun i e -> (i,e)) a
  |> Seq.filter (fun (i,e) -> i < at || i >= at + n)
  |> Seq.map (fun (i,e) -> e)
  |> Array.ofSeq

let asl v s = v <<< s 
(* let asr v s = v >>> s *)

let listHeadOption l =
  match l with
  | hd :: tl -> Some hd
  | _ -> None

let (&>) (self : 'v) (todo : Action<'id,'a> list) : ('v * Action<'id,'a> list) =
  (self, todo)

let (&+) ((s,a) : ('v * Action<'id,'a> list)) (f : 'v -> ('v * Action<'id,'a> list)) : ('v * Action<'id,'a> list) =
  let (ns,na) = f s in
  (ns,a @ na)

let init localNodeId =
  { localNodeId = localNodeId
  ; bucket = Some [||]
  ; dontSplit = false
  ; low = None
  ; high = None
  }


(* Returns the index of the contact if it exists *)
let indexOf ops id self =
  match self.bucket with
  | Some bucket ->
     bucket
     |> Seq.mapi (fun i c -> (i,c))
     |> Seq.skipWhile (fun (i,c) -> not (ops.idEqual id (ops.nodeId c)))
     |> Seq.truncate 1
     |> Seq.map (fun (i,c) -> i)
     |> List.ofSeq
     |> listHeadOption
     |> optionDefault -1
  | _ -> -1

(* We can compare arrays of ints so we'll express the distance that way for lack of anything
 * better for now. *)
let defaultDistance (ops : KBucketAbstract<'id,'a>) (firstId : 'id) (secondId : 'id) =
  let fl = ops.keyLength firstId in
  let sl = ops.keyLength secondId in
  let (longer,shorter) = if fl > sl then (firstId,secondId) else (secondId,firstId) in
  let ll = ops.keyLength longer in
  let sl = ops.keyLength shorter in
  let accumulator = Array.zeroCreate ll in
  for i = 0 to ll do
    begin
      let idx = ll - i - 1 in
      if i >= sl then
        accumulator.[idx] <- 255
      else
        accumulator.[idx] <- ((ops.keyNth i firstId) ^^^ (ops.keyNth i secondId))
    end ;
  accumulator

(* Determines whether the id at the bitIndex is 0 or 1. If 0, returns -1, else 1
// id: a Buffer to compare localNodeId with
// bitIndex: the bitIndex to which bit to check in the id Buffer
*)
let determineBucket (ops : KBucketAbstract<'id,'a>) (self : KBucket<'id,'a>) (id : 'id) (bitIndexOpt : int option) =
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
  let bitIndexWithinByte = bitIndex % 8 in
  
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
    let bit = (asl 1 (7 - bitIndexWithinByte)) in
    let bitValue = (byteUnderConsideration &&& bit) <> 0 in
    
    if bitValue then
      1
    else
      -1
      
type InternalOps<'id,'a> =
  { update : KBucketAbstract<'id,'a> -> KBucket<'id,'a> -> 'a -> int -> (KBucket<'id,'a> * Action<'id,'a> list) ;
    splitAndAddInternal : KBucketAbstract<'id,'a> -> InternalOps<'id,'a> -> KBucket<'id,'a> -> 'a -> int option -> (KBucket<'id,'a> * Action<'id,'a> list) ;
    addInternal : KBucketAbstract<'id,'a> -> InternalOps<'id,'a> -> KBucket<'id,'a> -> 'a -> int option -> (KBucket<'id,'a> * Action<'id,'a> list)
  }

let rec addInternal
      (ops : KBucketAbstract<'id,'a>)
      (intr : InternalOps<'id,'a>)
      (self : KBucket<'id,'a>)
      (contact : 'a)
      (bitIndexOpt : int option) : (KBucket<'id,'a> * Action<'id,'a> list) =
  let bitIndex = bitIndexOpt |> optionDefault 0 in
  let bitIndexPP = bitIndex + 1 in
  match (self.bucket, self.low, self.high) with
  | (None, Some low, Some high) ->
      if (determineBucket ops self (ops.nodeId contact) (Some (bitIndex))) < 0 then
        (* this is not a leaf node but an inner node with 'low' and 'high'
        // branches; we will check the appropriate bit of the identifier and
        // delegate to the appropriate node for further processing
        *)
        intr.addInternal ops intr low contact (Some bitIndexPP) &+
          (fun low ->
            { self with low = Some low } &> []
          )
      else
        intr.addInternal ops intr high contact (Some bitIndexPP) &+
          (fun high ->
            { self with high = Some high } &> []
          )
  | (Some bucket, _, _) ->
     (* Check if the contact already exists *)
     let index = indexOf ops (ops.nodeId contact) self in
     if index >= 0 then
       intr.update ops self contact index
     else
       (* The bucket is full *)
       if Array.length bucket < Constants.DEFAULT_NUMBER_OF_NODES_PER_K_BUCKET then
         { self with bucket = Some (Array.append bucket [|contact|]) } &> []
       else if self.dontSplit then
         (* we are not allowed to split the bucket
         // we need to ping the first constants.DEFAULT_NUMBER_OF_NODES_TO_PING
         // in order to determine if they are alive
         // only if one of the pinged nodes does not respond, can the new contact
         // be added (this prevents DoS flodding with new invalid contacts)
         *)
         self &>
           [Ping (Array.sub bucket 0 Constants.DEFAULT_NUMBER_OF_NODES_TO_PING, contact)]
       else
         intr.splitAndAddInternal ops intr self contact (Some bitIndex)
  | _ -> failwith "Expected bucket or low,high"

(* contact: Object *required* contact object
// id: Buffer *require* node id
// n: Integer *required* maximum number of closest contacts to return
// bitIndex: Integer (Default: 0)
// Return: Array of maximum of `n` closest contacts to the `contact`
*)
let rec closest
      (ops : KBucketAbstract<'id,'a>)
      (self : KBucket<'id,'a>)
      (contact : 'id)
      (n : int)
      (bitIndexOpt : int option) : 'a array =
  let bitIndex = bitIndexOpt |> optionDefault 0 in
  match (self.bucket, self.low, self.high) with
  | (None, Some low, Some high) ->
     let contacts = ref [||] in
     let _ =
       if determineBucket ops self contact (Some bitIndex) < 0 then
         begin
           let contactsVal = closest ops low contact n (Some bitIndex) in
           if Array.length contactsVal < n then
             let contactsVal2 = closest ops high contact n (Some bitIndex) in
             contacts := Array.append !contacts (Array.append contactsVal contactsVal2)
           else
             contacts := Array.append !contacts contactsVal
         end
       else
         begin
           let contactsVal = closest ops high contact n (Some bitIndex) in
           if Array.length contactsVal < n then
             let contactsVal2 = closest ops low contact n (Some bitIndex) in
             contacts := Array.append !contacts (Array.append contactsVal contactsVal2)
           else
             contacts := Array.append !contacts contactsVal
         end
     in
     (Array.sub !contacts 0 n)
  | (Some bucket, _, _) ->
     let distances =
       Array.map
         (fun storedContact ->
           (storedContact, ops.distance ops (ops.nodeId storedContact) contact)
         )
         bucket
     in
     let sorted =
       Array.sortWith
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
  | _ -> failwith "Expected bucket or low and high"
                  
(* Counts the number of contacts recursively.
// If this is a leaf, just return the number of contacts contained. Otherwise,
// return the length of the high and low branches combined.
*)
let rec count (self : KBucket<'id,'a>) =
  match (self.bucket, self.low, self.high) with
  | (Some bucket, _, _) ->
    Array.length bucket
  | (_, Some low, Some high) ->
    (count high) + (count low)
  | _ -> failwith "Bucket or Low and High expected"

(* Get a contact by its exact ID.
// If this is a leaf, loop through the bucket contents and return the correct
// contact if we have it or null if not. If this is an inner node, determine
// which branch of the tree to traverse and repeat.
// id: *required* a Buffer specifying the ID of the contact to fetch
// bitIndex: the bitIndex to which bit to check in the Buffer for navigating
//           the binary tree
*)
let rec get (ops : KBucketAbstract<'id,'a>) (self : KBucket<'id,'a>) (id : 'id) (bitIndexOpt : int option) =
  let bitIndex = bitIndexOpt |> optionDefault 0 in
  let bitIndexPP = bitIndex + 1 in

  match (self.bucket, self.low, self.high) with
  | (None, Some low, Some high) ->
      if determineBucket ops self id (Some bitIndex) < 0 then
        get ops low id (Some bitIndexPP)
      else
        get ops high id (Some bitIndexPP)
  | (Some bucket, _, _) ->
     let index = indexOf ops id self in (* index of uses contact.id for matching *)

     if index < 0 then
       None (* contact not found *)
     else
       Some bucket.[index]
  | _ -> failwith "Expected bucket or low and high"

(* contact: *required* the contact object to remove
// bitIndex: the bitIndex to which bit to check in the Buffer for navigating
//           the binary tree
*)
let rec remove (ops : KBucketAbstract<'id,'a>) (self : KBucket<'id,'a>) (contact : 'id) (bitIndexOpt : int option) =
  (* first check whether we are an inner node or a leaf (with bucket contents) *)
  match (self.bucket, self.low, self.high) with
  | (None, Some low, Some high) ->
     (* this is not a leaf node but an inner node with 'low' and 'high'
     // branches; we will check the appropriate bit of the identifier and
     // delegate to the appropriate node for further processing
     *)
     let bitIndex = bitIndexOpt |> optionDefault 0 in
     let bitIndexPP = bitIndex + 1 in

     if determineBucket ops self contact (Some bitIndex) < 0 then
       remove ops low contact (Some bitIndexPP)
     else
       remove ops high contact (Some bitIndexPP)
  | (Some bucket, _, _) ->
     let index = indexOf ops contact self in
     if index >= 0 then
       { self with bucket = Some (arrayRemove index 1 bucket) } &> []
     else
       self &> []
  | _ -> failwith "Expected bucket or high and low"

(* Splits the bucket, redistributes contacts to the new buckets, and marks the
// bucket that was split as an inner node of the binary tree of buckets by
// setting self.bucket = undefined;
// contact: *required* the contact object to add
// bitIndex: the bitIndex to which byte to check in the Buffer for navigating the
//          binary tree
*)
let rec splitAndAddInternal
    (ops : KBucketAbstract<'id,'a>)
    (intr : InternalOps<'id,'a>)
    (self : KBucket<'id,'a>)
    (contact : 'a)
    (bitIndexOpt : int option) =
  let bitIndex = bitIndexOpt |> optionDefault 0 in
  match (self.bucket, self.low, self.high) with
  | (Some bucket, None, None) ->
     let newLowBucket =
       Array.filter
         (fun storedContact ->
           determineBucket ops self (ops.nodeId storedContact) (Some bitIndex) < 0
         )
         bucket
     in
     let newHighBucket =
       Array.filter
         (fun storedContact ->
           determineBucket ops self (ops.nodeId storedContact) (Some bitIndex) >= 0
         )
         bucket
     in
     (* don't split the "far away" bucket
     // we check where the local node would end up and mark the other one as
     // "dontSplit" (i.e. "far away")
     *)
     let whichBucketIsFar =
       determineBucket ops self self.localNodeId (Some bitIndex) < 0
     in
     let splitSelf =
       { self with
           bucket = None ;
           low =
             Some
               { init self.localNodeId with
                   bucket = Some newLowBucket ;
                   dontSplit = not whichBucketIsFar
               } ;
           high =
             Some
               { init self.localNodeId with
                   bucket = Some newHighBucket ;
                   dontSplit = whichBucketIsFar
               }
       }
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
let rec toArray (self : KBucket<'id,'a>) : 'a array =
  match (self.bucket, self.low, self.high) with
  | (Some bucket, _, _) -> bucket
  | (_, Some low, Some high) -> Array.append (toArray low) (toArray high)
  | (_, _, _) -> failwith "Need bucket or low and high"

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
let update (ops : KBucketAbstract<'id,'a>) (self : KBucket<'id,'a>) (contact : 'a) (index : int) =
  match self.bucket with
  | Some bucket ->
     begin
       let _ =
         if not (ops.idEqual (ops.nodeId bucket.[index]) (ops.nodeId contact)) then
           failwith "indexOf() calculation resulted in wrong index"
       in
       let incumbent = bucket.[index] in
       let selection = ops.arbiter incumbent contact in
       if selection = incumbent && incumbent <> contact then
         (* if the selection is our old contact and the candidate is some new
         // contact, then there is nothing to do
         *)
         self &> []
       else
         { self with bucket = Some (Array.append (arrayRemove index 1 bucket) [|selection|]) } &> []
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
      (ops : KBucketAbstract<'id,'a>)
      (self : KBucket<'id,'a>)
      (contact : 'a)
      (bitIndexOpt : int option) : (KBucket<'id,'a> * Action<'id,'a> list) =
  addInternal ops internalOps_ self contact bitIndexOpt

let splitAndAdd
    (ops : KBucketAbstract<'id,'a>)
    (self : KBucket<'id,'a>)
    (contact : 'a)
    (bitIndexOpt : int option) =
  splitAndAddInternal ops internalOps_  self contact bitIndexOpt
