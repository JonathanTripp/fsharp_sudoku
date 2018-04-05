module oset

open Sset

let inline setElemCompare (x:^T) (y:^T) : Ordering =
    (^T: (static member setElemCompare: ^T -> ^T -> Ordering) (x, y))

type OSet<'T> = OSet of 'T list

[<RequireQualifiedAccess>]
module OSet =
    let inline ofList (l : ^T list) : OSet<'T> =
        l |> Sset.setify setElemCompare |> OSet

    let toList (OSet o : OSet<'T>) : 'T list =
        o

    let inline choose (chooser : 'T -> 'U option) (o : OSet<'T>) : OSet<'U> =
        o |> toList |> List.choose chooser |> ofList

    let inline concat (ds : OSet<OSet<'T>>) : OSet<'T> =
        ds |> toList |> List.map toList |> Sset.unions setElemCompare |> ofList

    let inline contains (element : 'T) (o : OSet<'T>) : bool =
        o |> toList |> Sset.contains setElemCompare element

    let count (o : OSet<'T>) : int =
        o |> toList |> List.length

    let inline difference (o : OSet<'T>) (o' : OSet<'T>) : OSet<'T> =
        Sset.subtract setElemCompare (o |> toList) (o' |> toList) |> ofList

    let empty : OSet<'T> = OSet []

    let exists (predicate : 'T -> bool) (o : OSet<'T>) : bool =
        o |> toList |> List.exists predicate 

    let inline filter (predicate : 'T -> bool) (o : OSet<'T>) : OSet<'T> =
        o |> toList |> List.filter predicate |> ofList

    let find (predicate : 'T -> bool) (o : OSet<'T>) : 'T =
        o |> toList |> List.find predicate

    let forall (predicate : 'T -> bool) (o : OSet<'T>) : bool =
        o |> toList |> List.forall predicate 

    let head (o : OSet<'T>) : 'T =
        o |> toList |> List.head

    let inline intersect (o : OSet<'T>) (o' : OSet<'T>) : OSet<'T> =
        Sset.intersect setElemCompare (o |> toList) (o' |> toList) |> ofList

    let inline isSubset (o : OSet<'T>) (o' : OSet<'T>) : bool =
        Sset.subset setElemCompare (o |> toList) (o' |> toList)

    let item (index : int) (o : OSet<'T>) : 'T =
        o |> toList |> List.item index

    let inline map (mapping : 'T -> 'U) (o : OSet<'T>) : OSet<'U> =
        o |> toList |> List.map mapping |> ofList

    let inline mapi (mapping : int -> 'T -> 'U) (o : OSet<'T>) : OSet<'U> =
        o |> toList |> List.mapi mapping |> ofList

    let inline range (first : int) (last : int) (fn : int -> 'T) : OSet<'T> =
        Sset.range first last |> List.map fn |> ofList

    let inline remove (value : 'T) (o : OSet<'T>) : OSet<'T> = 
        o |> toList |> Sset.remove setElemCompare value |> ofList

    let singleton (value : 'T) : OSet<'T> =
        List.singleton value |> OSet

    let inline skip (count : int) (o : OSet<'T>) : OSet<'T> =
        o |> toList |> List.skip count |> ofList

    let inline subsets (size : int) (o : OSet<'T>) : OSet<'T> list =
        Sset.setSubsets (o |> toList) size
        |> List.map ofList

    let inline take (count : int) (o : OSet<'T>) : OSet<'T> =
        o |> toList |> List.take count |> ofList

    let toString (o : OSet<'T>) : string =
        let toString a = a.ToString()
        let sb = new System.Text.StringBuilder()
        sb.Append("{")
          .Append(String.concat "," (o |> toList |> List.map toString))
          .Append("}")
          .ToString()

    let inline union (o : OSet<'T>) (o' : OSet<'T>) : OSet<'T> =
        Sset.union setElemCompare (o |> toList) (o' |> toList) |> ofList

    let inline unionMany (ds : OSet<'T> list) : OSet<'T> =
        ds |> List.map toList |> Sset.unions setElemCompare |> ofList
