module oset

open Sset

module SetElemComparers =
    let ht = new System.Collections.Generic.Dictionary<System.Type, obj>()

    let Get<'T> () : 'T -> 'T -> Ordering =
        let ty = typeof<'T>
        if ht.ContainsKey(ty) then
            match ht.[ty] with
            | :? ('T -> 'T -> Ordering) as r -> r
            | _ -> invalidArg "ty" ("The type "+ty.Name+" has a numeric association but it was not of the correct type")
        else
            invalidArg "ty" ("The type "+ty.Name+" does not have a setElemComparer")

    let Register (r : 'T -> 'T -> Ordering) : unit =
        ht.Add(typeof<'T>, box r)

[<NoComparison;NoEquality>]
type OSet<'T> =
    { data : 'T list;
      setElemCompare : 'T -> 'T -> Ordering; }

[<RequireQualifiedAccess>]
module OSet =
    let ofList' (setElemCompare: 'T -> 'T -> Ordering) (l :'T list) : OSet<'T> =
        let setElemCompare = SetElemComparers.Get<'T>() in
        let data = l |> Sset.setify setElemCompare in
        { data = data; setElemCompare = setElemCompare }

    let ofList (l :'T list) : OSet<'T> =
        let setElemCompare = SetElemComparers.Get<'T>() in
        ofList' setElemCompare l

    let toList (oset : OSet<'T>) :'T list =
        oset.data

    let choose (chooser :'T-> 'U option) (o : OSet<'T>) : OSet<'U> =
        o |> toList |> List.choose chooser |> ofList

    let concat (ds : OSet<'T> list) : OSet<'T> =
        let setElemCompare = SetElemComparers.Get<'T>() in
        ds |> List.map toList |> Sset.unions setElemCompare |> ofList' setElemCompare

    let contains (element : 'T) (o : OSet<'T>) : bool =
        let setElemCompare = SetElemComparers.Get<'T>() in
        o |> toList |> Sset.contains setElemCompare element

    let count (o : OSet<'T>) : int =
        o |> toList |> List.length

    let difference (o : OSet<'T>) (o' : OSet<'T>) : OSet<'T> =
        let setElemCompare = SetElemComparers.Get<'T>() in
        Sset.subtract setElemCompare (o |> toList) (o' |> toList) |> ofList' setElemCompare

    let empty () : OSet<'T> = ofList []

    let equals (lhs : OSet<'T>) (rhs : OSet<'T>) : bool =
        let setElemCompare = SetElemComparers.Get<'T>() in
        let l1 = toList lhs in
        let l2 = toList rhs in

        List.length l1 = List.length l2 &&
        List.forall2 (fun element1 element2 -> setElemCompare element1 element2 = EQ) l1 l2

    let exists (predicate :'T-> bool) (o : OSet<'T>) : bool =
        o |> toList |> List.exists predicate 

    let filter (predicate :'T-> bool) (o : OSet<'T>) : OSet<'T> =
        o |> toList |> List.filter predicate |> ofList

    let find (predicate :'T-> bool) (o : OSet<'T>) :'T=
        o |> toList |> List.find predicate

    let forall (predicate :'T-> bool) (o : OSet<'T>) : bool =
        o |> toList |> List.forall predicate 

    let head (o : OSet<'T>) :'T=
        o |> toList |> List.head

    let intersect (o : OSet<'T>) (o' : OSet<'T>) : OSet<'T> =
        let setElemCompare = SetElemComparers.Get<'T>() in
        Sset.intersect setElemCompare (o |> toList) (o' |> toList) |> ofList' setElemCompare

    let isSubset (o : OSet<'T>) (o' : OSet<'T>) : bool =
        let setElemCompare = SetElemComparers.Get<'T>() in
        Sset.subset setElemCompare (o |> toList) (o' |> toList)

    let item (index : int) (o : OSet<'T>) :'T=
        o |> toList |> List.item index

    let map (mapping :'T-> 'U) (o : OSet<'T>) : OSet<'U> =
        o |> toList |> List.map mapping |> ofList

    let mapi (mapping : int ->'T-> 'U) (o : OSet<'T>) : OSet<'U> =
        o |> toList |> List.mapi mapping |> ofList

    let print (printer : 'T -> string) (o : OSet<'T>) : string =
        let sb = new System.Text.StringBuilder()
        sb.Append("{")
          .Append(String.concat "," (o |> toList |> List.map printer))
          .Append("}")
          .ToString()

    let range (first : int) (last : int) (fn : int -> 'T) : OSet<'T> =
        Sset.range first last |> List.map fn |> ofList

    let remove (value : 'T) (o : OSet<'T>) : OSet<'T> = 
        let setElemCompare = SetElemComparers.Get<'T>() in
        o |> toList |> Sset.remove setElemCompare value |> ofList' setElemCompare

    let singleton (value : 'T) : OSet<'T> =
        List.singleton value |> ofList

    let skip (count : int) (o : OSet<'T>) : OSet<'T> =
        o |> toList |> List.skip count |> ofList

    let subsets (size : int) (o : OSet<'T>) : OSet<'T> list =
        Sset.setSubsets (o |> toList) size
        |> List.map ofList

    let take (count : int) (o : OSet<'T>) : OSet<'T> =
        o |> toList |> List.take count |> ofList

    let union (o : OSet<'T>) (o' : OSet<'T>) : OSet<'T> =
        let setElemCompare = SetElemComparers.Get<'T>() in
        Sset.union setElemCompare (o |> toList) (o' |> toList) |> ofList' setElemCompare

    let unionMany (ds : OSet<'T> list) : OSet<'T> =
        let setElemCompare = SetElemComparers.Get<'T>() in
        ds |> List.map toList |> Sset.unions setElemCompare |> ofList' setElemCompare
