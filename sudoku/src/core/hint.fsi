module core.Hint

open Sudoku
open compat.oset
open compat.smap

[<NoComparison; NoEquality>]
type description = 
    { primaryHouses : houses;
      secondaryHouses : houses;
      candidateReductions : candidateReduction list;
      setCellValueAction : value option;
      pointers : candidateReduction list;
      focus : digits }

module Description =
    val print : description -> string

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
