module core.Hint

open Sudoku
open oset
open smap

type description = 
    { primaryHouses : houses;
      secondaryHouses : houses;
      candidateReductions : OSet<candidateReduction>;
      setCellValueAction : value option;
      pointers : OSet<candidateReduction>;
      focus : digits }
    interface OSetMember<description>

module Description =
    val to_string : description -> string

(* To draw a cell we may want to display extra information... *)
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

type description2 = 
    { annotations : SMap<cell, annotation> }

val mhas : solution -> Puzzlemap.puzzleMap -> description -> description2

val mhas2 : solution -> Puzzlemap.puzzleMap -> description2
