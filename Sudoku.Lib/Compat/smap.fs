module Sudoku.Lib.compat.smap

open Sudoku.Lib.compat.Sset
open Sudoku.Lib.compat.oset

[<NoComparison;NoEquality>]
type SMap<'Key, 'Value> =
    | SMap of ('Key * 'Value) list

[<RequireQualifiedAccess>]
module SMap =
    let ofList (elements : ('Key * 'Value) list) : SMap<'Key, 'Value> =
        elements |> SMap

    let toList (SMap elements : SMap<'Key, 'Value>) : ('Key * 'Value) list =
        elements

    let ofLookup (fn : 'Key -> 'Value) (cs : OSet<'Key>) : SMap<'Key, 'Value> =
        cs |> OSet.toList |> List.map (fun c -> (c, fn c)) |> ofList

    let get' (setElemCompare : 'Key -> 'Key -> Ordering) (key : 'Key) (m : SMap<'Key, 'Value>) : 'Value =
        m |> toList |> List.find (fun a -> setElemCompare key (fst a) = EQ) |> snd

    let get (key : 'Key) (m : SMap<'Key, 'Value>) : 'Value =
        let setElemCompare = SetElemComparers.Get<'Key>() in
        get' setElemCompare key m

    let tryGet (key : 'Key) (m : SMap<'Key, 'Value>) : 'Value option =
        let setElemCompare = SetElemComparers.Get<'Key>() in
        let sm = toList m in
        if List.exists (fun a -> setElemCompare key (fst a) = EQ) sm then Some (get' setElemCompare key m)
        else None