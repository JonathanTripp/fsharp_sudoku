module Sudoku.Lib.core.Hint

open Sudoku.Lib.compat.smap

open Sudoku.Lib.core.Sudoku

[<NoComparison; NoEquality>]
type description = 
    { primaryHouses : houses;
      secondaryHouses : houses;
      candidateReductions : candidateReductions;
      setCellValueAction : value option;
      pointers : candidateReductions;
      focus : digits }

type descriptions = description list

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
