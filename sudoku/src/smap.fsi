module smap

open oset

[<NoComparison;NoEquality>]
type SMap<'Key, 'Value> =
    | SMap of ('Key * 'Value) list

[<RequireQualifiedAccess>]
module SMap =
    val ofList : ('Key * 'Value) list -> SMap<'Key, 'Value>
    val ofLookup : ('Key -> 'Value) -> OSet<'Key> -> SMap<'Key, 'Value>
    val get : 'Key -> SMap<'Key, 'Value> -> 'Value
    val tryGet : 'Key -> SMap<'Key, 'Value> -> 'Value option
