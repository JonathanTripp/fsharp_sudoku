module oset

type OSet<[<EqualityConditionalOn>]'T when 'T : comparison> =
    | SSet of Set<'T>

[<RequireQualifiedAccess>]
module OSet =
    let ofSet (o : Set<'T>) : OSet<'T> =
        o |> SSet

    let toSet (SSet o : OSet<'T>) : Set<'T> =
        o

    let ofList (l : 'T list) : OSet<'T> =
        l |> Set.ofList |> ofSet

    let toList (o : OSet<'T>) : 'T list =
        o |> toSet |> Set.toList

    let choose (chooser : 'T -> 'U option) (o : OSet<'T>) : OSet<'U> =
        o |> toList |> List.choose chooser |> ofList

    let concat (ds : OSet<OSet<'T>>) : OSet<'T> =
        ds |> toList |> List.map toSet |> Set.unionMany |> ofSet

    let contains (element : 'T) (o : OSet<'T>) : bool =
        o |> toSet |> Set.contains element

    let count (o : OSet<'T>) : int =
        o |> toSet |> Set.count

    let difference (o : OSet<'T>) (o' : OSet<'T>) : OSet<'T> =
        Set.difference (o |> toSet) (o' |> toSet) |> ofSet

    let empty : OSet<'T> = SSet (Set.empty)

    let exists (predicate : 'T -> bool) (o : OSet<'T>) : bool =
        o |> toSet |> Set.exists predicate 

    let filter (predicate : 'T -> bool) (o : OSet<'T>) : OSet<'T> =
        o |> toSet |> Set.filter predicate |> ofSet

    let find (predicate : 'T -> bool) (o : OSet<'T>) : 'T =
        o |> toList |> List.find predicate

    let forall (predicate : 'T -> bool) (o : OSet<'T>) : bool =
        o |> toSet |> Set.forall predicate 

    let head (o : OSet<'T>) : 'T =
        o |> toList |> List.head

    let intersect (o : OSet<'T>) (o' : OSet<'T>) : OSet<'T> =
        Set.intersect (o |> toSet) (o' |> toSet) |> ofSet

    let isSubset (o : OSet<'T>) (o' : OSet<'T>) : bool =
        Set.isSubset (o |> toSet) (o' |> toSet)

    let item (index : int) (o : OSet<'T>) : 'T =
        o |> toList |> List.item index

    let map (mapping : 'T -> 'U) (o : OSet<'T>) : OSet<'U> =
        o |> toSet |> Set.map mapping |> ofSet

    let mapi (mapping : int -> 'T -> 'U) (o : OSet<'T>) : OSet<'U> =
        o |> toList |> List.mapi mapping |> ofList

    let remove (value : 'T) (o : OSet<'T>) : OSet<'T> = 
        o |> toSet |> Set.remove value |> ofSet

    let singleton (value : 'T) : OSet<'T> =
        Set.singleton value |> ofSet

    let skip (count : int) (o : OSet<'T>) : OSet<'T> =
        o |> toList |> List.skip count |> ofList

    let take (count : int) (o : OSet<'T>) : OSet<'T> =
        o |> toList |> List.take count |> ofList

    let toString (o : OSet<'T>) : string =
        let toString a = a.ToString()
        let sb = new System.Text.StringBuilder()
        sb.Append("{")
          .Append(String.concat "," (o |> toList |> List.map toString))
          .Append("}")
          .ToString()

    let union (o : OSet<'T>) (o' : OSet<'T>) : OSet<'T> =
        Set.union (o |> toSet) (o' |> toSet) |> ofSet

    let unionMany (ds : OSet<'T> list) : OSet<'T> =
        ds |> List.map toSet |> Set.unionMany |> ofSet
