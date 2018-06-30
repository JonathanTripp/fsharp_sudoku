module Sudoku.Lib.compat.Sset

type Ordering = LT|EQ|GT

(*
Sset is intended to be an ordered list, without duplicates.

Bits borrowed from:
https://www.cl.cam.ac.uk/~jrh13/atp/OCaml/lib.ml
https://hackage.haskell.org/package/base-4.8.2.0/docs/src/GHC.List.html
*)

let rec funpow n f x =
  if n < 1 then x else funpow (n-1) f (f x)

(*
let rec funpow2 (n : int) (f : 'a -> unit) (x : 'a) =
  if n < 1 then x else f(x); funpow2 (n-1) f x
*)
let rec take (n : int) (l : 'a list) : 'a list =
    if n <= 0 then []
    else
        match l with
        | [] -> []
        | x :: xs -> x :: take (n - 1) xs

let rec drop (n : int) (l : 'a list) : 'a list =
    if n <= 0 then l
    else
        match l with
        | [] -> []
        | _::t  when n = 1 -> t
        | h::t -> drop (n-1) t

let rec uniq (comparer:'a->'a->Ordering) (l : 'a list) : 'a list =
    match l with
    | [] -> []
    | x :: (y :: _ as tl) when comparer x y = EQ -> (uniq comparer tl)
    | hd :: tl -> hd :: (uniq comparer tl)

(* ------------------------------------------------------------------------- *)
(* Merging of sorted lists (maintaining repetitions).                        *)
(* ------------------------------------------------------------------------- *)

let rec merge ord l1 l2 =
  match l1 with
  | [] -> l2
  | h1::t1 -> match l2 with
              | [] -> l1
              | h2::t2 -> if ord h1 h2 then h1::(merge ord t1 l2)
                          else h2::(merge ord l1 t2)

let map f =
  let rec mapf l =
    match l with
      [] -> []
    | (x::t) -> let y = f x in y::(mapf t) in
  mapf

(* ------------------------------------------------------------------------- *)
(* Bottom-up mergesort.                                                      *)
(* ------------------------------------------------------------------------- *)
let rec mergepairs ord l1 l2 =
    match (l1,l2) with
    | ([s],[]) -> s
    | (l,[]) -> mergepairs ord [] l
    | (l,[s1]) -> mergepairs ord (s1::l) []
    | (l,(s1::s2::ss)) -> mergepairs ord ((merge ord s1 s2)::l) ss

let sort ord l =
    match l with
    | [] -> []
    | _ -> mergepairs ord [] (map (fun x -> [x]) l)

let rec canonical (comparer:'a->'a->Ordering) lis =
    match lis with
    | x :: (y :: _ as rest) -> (comparer x y = LT) && canonical comparer rest
    | _ -> true

let setify (comparer:'a->'a->Ordering) (l : 'a list) : 'a list =
  if canonical comparer l then l
  else uniq comparer (sort (fun x y -> comparer x y <> GT) l)

let union (comparer:'a->'a->Ordering) =
  let rec union l1 l2 =
    match (l1,l2) with
      | ([],l2) -> l2
      | (l1,[]) -> l1
      | ((h1::t1 as l1),(h2::t2 as l2)) ->
          if comparer h1 h2 = EQ then h1::(union t1 t2)
          else if comparer h1 h2 = LT then h1::(union t1 l2)
          else h2::(union l1 t2) in
  fun s1 s2 -> union (setify comparer s1) (setify comparer s2);;

let intersect (comparer:'a->'a->Ordering) =
  let rec intersect l1 l2 =
    match (l1,l2) with
        ([],l2) -> []
      | (l1,[]) -> []
      | ((h1::t1 as l1),(h2::t2 as l2)) ->
          if comparer h1 h2 = EQ then h1::(intersect t1 t2)
          else if comparer h1 h2 = LT then intersect t1 l2
          else intersect l1 t2 in
  fun s1 s2 -> intersect (setify comparer s1) (setify comparer s2);;

let subtract (comparer:'a->'a->Ordering) =
  let rec subtract l1 l2 =
    match (l1,l2) with
        ([],l2) -> []
      | (l1,[]) -> l1
      | ((h1::t1 as l1),(h2::t2 as l2)) ->
          if comparer h1 h2 = EQ then subtract t1 t2
          else if comparer h1 h2 = LT then h1::(subtract t1 l2)
          else subtract l1 t2 in
  fun s1 s2 -> subtract (setify comparer s1) (setify comparer s2);;

(*
let subset,psubset =
  let rec subset l1 l2 =
    match (l1,l2) with
        ([],l2) -> true
      | (l1,[]) -> false
      | (h1::t1,h2::t2) ->
          if h1 = h2 then subset t1 t2
          else if h1 < h2 then false
          else subset l1 t2
  and psubset l1 l2 =
    match (l1,l2) with
        (l1,[]) -> false
      | ([],l2) -> true
      | (h1::t1,h2::t2) ->
          if h1 = h2 then psubset t1 t2
          else if h1 < h2 then false
          else subset l1 t2 in
  (fun s1 s2 -> subset (setify comparer s1) (setify comparer s2)),
  (fun s1 s2 -> psubset (setify comparer s1) (setify comparer s2));;
*)
let insert (comparer : 'a->'a->Ordering) x s = union comparer [x] s;;

let image (comparer : 'a->'a->Ordering) f s = setify comparer (map f s);;

(* ------------------------------------------------------------------------- *)
(* Union of a family of sets.                                                *)
(* ------------------------------------------------------------------------- *)
let rec itlist f l b =
  match l with
    [] -> b
  | (h::t) -> f h (itlist f t b)

let unions (comparer : 'a->'a->Ordering) (s : 'a list list) = setify comparer (itlist (@) s [])

let contains (comparer : 'a->'a->Ordering) (t : 'a) (s : 'a list) : bool =
    List.exists (fun t' -> comparer t' t = EQ) s

let remove (comparer : 'a->'a->Ordering) (t : 'a) (s : 'a list) : 'a list =
    List.filter (fun t' -> comparer t' t <> EQ) s

let subset (comparer : 'a->'a->Ordering) (s1 : 'a list) (s2 : 'a list) =
  let rec subset l1 l2 =
    match (l1,l2) with
        ([],l2) -> true
      | (l1,[]) -> false
      | (h1::t1,h2::t2) ->
          if comparer h1 h2 = EQ then subset t1 t2
          else if comparer h1 h2 = LT then false
          else subset l1 t2
     in
  subset (setify comparer s1) (setify comparer s2)

let rec choose f l =
  match l with
    [] -> []
  | (h::t) -> let rest = choose f t in
              match f h with
              | Some x -> x :: rest
              | None -> rest

let rec mapfilter f l =
  match l with
    [] -> []
  | (h::t) -> let rest = mapfilter f t in
              try (f h)::rest with Failure _ -> rest;;

let rec doSetSubsets (list : 'a list) (size : int) (prefix : 'a list) : 'a list list = 
    match list with
    | x :: xs when size > 0 -> 
        if size = 1 then (x :: prefix) :: doSetSubsets xs 1 prefix
        else 
            let inc = doSetSubsets xs (size - 1) (x :: prefix) in
            let dec = doSetSubsets xs size prefix in

            List.append inc dec
    | _ -> []

let rec setSubsets (as' : 'a list) (size : int) : 'a list list =
    doSetSubsets as' size []

(*
    let s0 = []
    let p00 = setSubsets s0 0
    let p01 = setSubsets s0 1
    let p02 = setSubsets s0 2

    let s1 = [ 1 ]
    let p10 = setSubsets s1 0
    let p11 = setSubsets s1 1
    let p12 = setSubsets s1 2
    let p13 = setSubsets s1 3

    let s2 = [ 1; 2 ]
    let p20 = setSubsets s2 0
    let p21 = setSubsets s2 1
    let p22 = setSubsets s2 2
    let p23 = setSubsets s2 3
    let p24 = setSubsets s2 4

    let s3 = [ 1; 2; 3 ]
    let p30 = setSubsets s3 0
    let p31 = setSubsets s3 1
    let p32 = setSubsets s3 2
    let p33 = setSubsets s3 3
    let p34 = setSubsets s3 4
    let p35 = setSubsets s3 5
*)

let rec range i j = if i > j then [] else i :: (range (i+1) j)

let id (x : 'a) : 'a = x

let rec zip l1 l2 =
  match (l1,l2) with
        ([],[]) -> []
      | (h1::t1,h2::t2) -> (h1,h2)::(zip t1 t2)
      | _ -> failwith "zip"

let explode (s : string) =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

(* https://www.rosettacode.org/wiki/Tokenize_a_string#OCaml *)
let succ i = i + 1

let split_char sep str =
  let string_index_from i =
    try Some (Sstring.index_from str i sep)
    with _ -> None
  in
  let rec aux i acc =
    match string_index_from i with
    | Some i' ->
        let w = Sstring.sub str i (i' - i) in
        aux (succ i') (w::acc)
    | None ->
        let w = Sstring.sub str i (String.length str - i) in
        List.rev (w::acc)
  in
  aux 0 []
