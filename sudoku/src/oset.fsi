module oset

type OSet<[<EqualityConditionalOn>]'T when 'T : comparison> =
    | SSet of Set<'T>

[<RequireQualifiedAccess>]
module OSet =
    val ofSet : Set<'T> -> OSet<'T>
    val toSet : OSet<'T> -> Set<'T>
    val ofList : List<'T> -> OSet<'T>
    val toList : OSet<'T> -> List<'T>

    val choose : ('T -> 'U option) -> OSet<'T> -> OSet<'U>
    val concat : OSet<OSet<'T>> -> OSet<'T>
    val contains : 'T -> OSet<'T> -> bool
    val count : OSet<'T> -> int
    val difference : OSet<'T> -> OSet<'T> -> OSet<'T>
    val empty : OSet<'T>
    val exists : ('T -> bool) -> OSet<'T> -> bool
    val filter : ('T -> bool) -> OSet<'T> -> OSet<'T>
    val find : ('T -> bool) -> OSet<'T> -> 'T
    val forall : ('T -> bool) -> OSet<'T> -> bool
    val head : OSet<'T> -> 'T
    val intersect : OSet<'T> -> OSet<'T> -> OSet<'T>
    val isSubset : OSet<'T> -> OSet<'T> -> bool
    val map : ('T -> 'U) -> OSet<'T> -> OSet<'U>
    val mapi : (int -> 'T -> 'U) -> OSet<'T> -> OSet<'U>
    val item : int -> OSet<'T> -> 'T
    val remove : 'T -> OSet<'T> -> OSet<'T>
    val singleton : 'T -> OSet<'T>
    val skip : int -> OSet<'T> -> OSet<'T>
    val take : int -> OSet<'T> -> OSet<'T>
    val toString : OSet<'T> -> string
    val union : OSet<'T> -> OSet<'T> -> OSet<'T>
    val unionMany : List<OSet<'T>> -> OSet<'T>
