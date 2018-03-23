module smap

open oset

type SMap<'Key, 'Value when 'Key : comparison> =
    | SMap of Map<'Key, 'Value>

[<RequireQualifiedAccess>]
module SMap =
    val ofList : ('Key * 'Value) list -> SMap<'Key, 'Value>
    val ofLookup : ('Key -> 'Value) -> OSet<'Key> -> SMap<'Key, 'Value>
    val get : 'Key -> SMap<'Key, 'Value> -> 'Value
    val tryGet : 'Key -> SMap<'Key, 'Value> -> 'Value option
