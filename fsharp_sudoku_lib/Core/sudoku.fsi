module Sudoku.Lib.core.Sudoku

open Sudoku.Lib.compat.Sset
open Sudoku.Lib.compat.oset
open Sudoku.Lib.compat.smap

type size = int
[<NoComparison;NoEquality>]
type column =
    | CColumn of int
module Column =
    val ofNat : int -> column
    val print : column -> string
    val setElemCompare : column ->column -> Ordering
type columns = OSet<column>
module Columns =
  val print : columns -> string
[<NoComparison;NoEquality>]
type row =
    | RRow of int
module Row =
    val ofNat : int -> row
    val print : row -> string
    val setElemCompare : row -> row -> Ordering
type rows = OSet<row>
module Rows =
  val print : rows -> string
[<NoComparison;NoEquality>]
type cell =
    {col: column;
     row: row;}
module Cell =
    val make : column -> row -> cell
    val print : cell -> string
    val setElemCompare : cell -> cell -> Ordering
type cells = OSet<cell>
module Cells =
  val print : cells -> string
[<NoComparison;NoEquality>]
type stack =
    | SStack of int
module Stack =
    val ofNat : int -> stack
    val print : stack -> string
    val setElemCompare : stack -> stack -> Ordering
type stacks = OSet<stack>
module Stacks =
    val print : stacks -> string
type boxWidth = int
[<NoComparison;NoEquality>]
type band =
    | BBand of int
module Band =
    val ofNat : int -> band
    val print : band -> string
    val setElemCompare : band -> band -> Ordering
type bands = OSet<band>
module Bands =
    val print : bands -> string
type boxHeight = int
[<NoComparison;NoEquality>]
type bbox =
    {stack: stack;
     band: band;}
module Box =
    val make : stack -> band -> bbox
    val print : bbox -> string
    val setElemCompare : bbox -> bbox -> Ordering
type boxes = OSet<bbox>
module Boxes =
    val print : boxes -> string
[<NoComparison;NoEquality>]
type line =
    | LColumn of column
    | LRow of row
[<NoComparison;NoEquality>]
type house =
    | HColumn of column
    | HRow of row
    | HBox of bbox
module House =
    val make_column : column -> house
    val make_row : row -> house
    val make_box : bbox -> house
    val print : house -> string
    val setElemCompare : house -> house -> Ordering
type houses = OSet<house>
module Houses =
    val print : houses -> string
[<NoComparison;NoEquality>]
type digit =
    | Digit of char
module Digit =
    val ofNat : int -> digit
    val print : digit -> string
    val setElemCompare : digit -> digit -> Ordering
type digits = OSet<digit>
module Digits =
    val print : digits -> string
[<NoComparison;NoEquality>]
type puzzleShape =
    {size: size;
     boxWidth: boxWidth;
     boxHeight: boxHeight;
     alphabet: digits;}
module PuzzleShape =
    val default' : puzzleShape
[<NoComparison;NoEquality>]
type cellContents =
    | BigNumber of digit
    | PencilMarks of digits
module CellContents =
    val make_big_number : digit -> cellContents
    val make_pencil_marks : digits -> cellContents
[<NoComparison;NoEquality>]
type value =
    {cell: cell;
     digit: digit;}
module Value =
    val make : cell -> digit -> value
    val print : value -> string
[<NoComparison;NoEquality>]
type candidate =
    {cell: cell;
     digit: digit;}
module Candidate =
    val make : cell -> digit -> candidate
    val print : candidate -> string
[<NoComparison;NoEquality>]
type candidateReduction =
    {cell: cell;
     candidates: digits;}
module CandidateReduction =
    val make : cell -> digits -> candidateReduction
    val print : candidateReduction -> string
type candidateReductions = candidateReduction list
module CandidateReductions =
    val print : candidateReductions -> string
[<NoComparison;NoEquality>]
type action =
    | Load of string
    | LoadEliminateAction
    | Placement of value
    | Eliminate of candidate
module Action =
  val print : action -> string
[<NoComparison;NoEquality>]
type given = SMap<cell, digit option>
[<NoComparison;NoEquality>]
type current = SMap<cell, cellContents>
[<NoComparison;NoEquality>]
type cellCandidates = SMap<cell, digits>
[<NoComparison;NoEquality>]
type solution =
    {given: given;
     current: current;
     steps: action list;}
module Solution =
    val givenToCurrent : cells -> given -> digits -> current
    val currentCellCandidates : cells -> current -> cellCandidates

val registerSetElemComparers : unit -> unit
