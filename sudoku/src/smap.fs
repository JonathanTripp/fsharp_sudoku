module smap

open oset

type SMap<'Key, 'Value when 'Key : comparison> =
    | SMap of Map<'Key, 'Value>

[<RequireQualifiedAccess>]
module SMap =
    let ofMap (m : Map<'Key, 'Value>) : SMap<'Key, 'Value> =
        SMap m

    let toMap (SMap m : SMap<'Key, 'Value>) : Map<'Key, 'Value> =
        m

    let ofList (elements : ('Key * 'Value) list) : SMap<'Key, 'Value> =
        elements |> Map.ofList |> ofMap

    let inline ofLookup (fn : ^Key -> 'Value) (cs : OSet< ^Key >) : SMap< ^Key, 'Value> =
        cs |> OSet.toList |> List.map (fun c -> (c, fn c)) |> ofList

    let get (key : 'Key) (m : SMap<'Key, 'Value>) : 'Value =
        let n = m |> toMap
        n.Item key

    let tryGet (key : 'Key) (m : SMap<'Key, 'Value>) : 'Value option =
        let n = m |> toMap
        n.TryFind key
