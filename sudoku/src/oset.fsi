module oset

open Sset

val inline setElemCompare : ^T * ^T -> Ordering
    when ^T : (static member setElemCompare: ^T * ^T -> Ordering)

type OSet<'T> = OSet of 'T list

[<RequireQualifiedAccess>]
module OSet =
    val inline ofList : ^T list -> OSet< ^T >
    val inline toList : OSet< ^T > -> ^T list

    val inline choose : (^T -> ^U option) -> OSet< ^T > -> OSet< ^U >
    val inline concat : OSet<OSet< ^T >> -> OSet< ^T >
    val inline contains : ^T -> OSet< ^T > -> bool
    val inline count : OSet< ^T > -> int
    val inline difference : OSet< ^T > -> OSet< ^T > -> OSet< ^T >
    val inline empty : unit -> OSet< ^T >
    val inline exists : (^T -> bool) -> OSet< ^T > -> bool
    val inline filter : (^T -> bool) -> OSet< ^T > -> OSet< ^T >
    val inline find : (^T -> bool) -> OSet< ^T > -> ^T
    val inline forall : (^T -> bool) -> OSet< ^T > -> bool
    val inline head : OSet< ^T > -> ^T
    val inline intersect : OSet< ^T > -> OSet< ^T > -> OSet< ^T >
    val inline isSubset : OSet< ^T > -> OSet< ^T > -> bool
    val inline item : int -> OSet< ^T > -> ^T
    val inline map : (^T -> ^U) -> OSet< ^T > -> OSet< ^U >
    val inline mapi : (int -> ^T -> ^U) -> OSet< ^T > -> OSet< ^U >
    val inline range : int -> int -> (int -> ^T) -> OSet< ^T >
    val inline remove : ^T -> OSet< ^T > -> OSet< ^T >
    val inline singleton : ^T -> OSet< ^T >
    val inline skip : int -> OSet< ^T > -> OSet< ^T >
    val inline subsets : int -> OSet< ^T > -> OSet< ^T > list
    val inline take : int -> OSet< ^T > -> OSet< ^T >
    val inline toString : OSet< ^T > -> string
    val inline union : OSet< ^T > -> OSet< ^T > -> OSet< ^T >
    val inline unionMany : OSet< ^T > list -> OSet< ^T >
