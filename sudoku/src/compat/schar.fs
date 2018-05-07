module compat.Schar

let chr (i : int) : char =
    i
    |> char

let code (c : char) : int =
    c
    |> int
