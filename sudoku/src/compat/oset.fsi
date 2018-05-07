module compat.oset

open compat.Sset

module SetElemComparers =
    val Get<'T> : unit -> ('T -> 'T -> Ordering)
    val Register : ('T -> 'T -> Ordering) -> unit

[<NoComparison;NoEquality>]
type OSet<'T> =
    { data : 'T list;
      setElemCompare : 'T -> 'T -> Ordering; }

[<RequireQualifiedAccess>]
module OSet =
    val ofList : 'T list -> OSet<'T>
    val toList : OSet<'T> ->'T list

    val choose : ('T ->'U option) -> OSet<'T> -> OSet<'U>
    val concat : OSet<'T> list -> OSet<'T>
    val contains : 'T-> OSet<'T> -> bool
    val count : OSet<'T> -> int
    val difference : OSet<'T> -> OSet<'T> -> OSet<'T>
    val empty : unit -> OSet<'T>
    val equals : OSet<'T> -> OSet<'T> -> bool
    val exists : ('T -> bool) -> OSet<'T> -> bool
    val filter : ('T -> bool) -> OSet<'T> -> OSet<'T>
    val find : ('T -> bool) -> OSet<'T> -> 'T
    val forall : ('T -> bool) -> OSet<'T> -> bool
    val head : OSet<'T> -> 'T
    val intersect : OSet<'T> -> OSet<'T> -> OSet<'T>
    val isSubset : OSet<'T> -> OSet<'T> -> bool
    val item : int -> OSet<'T> -> 'T
    val map : ('T -> 'U) -> OSet<'T> -> OSet<'U>
    val mapi : (int ->'T-> 'U) -> OSet<'T> -> OSet<'U>
    val print : ('T -> string) -> OSet<'T> -> string
    val range : int -> int -> (int -> 'T) -> OSet<'T>
    val remove : 'T-> OSet<'T> -> OSet<'T>
    val singleton :'T-> OSet<'T>
    val skip : int -> OSet<'T> -> OSet<'T>
    val subsets : int -> OSet<'T> -> OSet<'T> list
    val take : int -> OSet<'T> -> OSet<'T>
    val union : OSet<'T> -> OSet<'T> -> OSet<'T>
    val unionMany : OSet<'T> list -> OSet<'T>
