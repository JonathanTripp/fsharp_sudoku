module core.Hint

open Sudoku
open oset
open smap

[<NoComparison; NoEquality>]
type description = 
    { primaryHouses : OSet<house>;
      secondaryHouses : OSet<house>;
      candidateReductions : candidateReduction list;
      setCellValueAction : value option;
      pointers : candidateReduction list;
      focus : digits }

module Description =
    val to_string : description -> string

(* To draw a cell we may want to display extra information... *)
[<NoComparison; NoEquality>]
type annotation = 
    { given : digit option;
      current: cellContents;
      setValue : digit option;
      primaryHintHouse : bool;
      secondaryHintHouse : bool;
      setValueReduction : digit option;
      reductions : digits;
      pointers : digits;
      focus : digits }

[<NoComparison; NoEquality>]
type description2 = 
    { annotations : SMap<cell, annotation> }

val mhas : solution -> Puzzlemap.puzzleMap -> description -> description2

val mhas2 : solution -> Puzzlemap.puzzleMap -> description2
