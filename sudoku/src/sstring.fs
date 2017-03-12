module Sstring

let compare (s1 : string) (s2 : string) : int =
    compare s1 s2

let get (s : string) (i: int) : char =
    s.Chars i

let index_from (s : string) (startIndex : int) (c : char) : int =
    s.IndexOf(c, startIndex)

let make (count : int) (c : char) =
    c
    |> string
    |> String.replicate count

let sub (s : string) (startIndex : int) (length : int) : string =
    s.Substring(startIndex, length)

