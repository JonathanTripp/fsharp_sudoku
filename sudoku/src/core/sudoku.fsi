module core.Sudoku

open Sset
open oset
open smap

type size = int
[<NoComparison;NoEquality>]
type column =
    | CColumn of int
module Column =
    val ofNat : int -> column
    val setElemCompare : column ->column -> Ordering
type columns = OSet<column>
module Columns =
  val toString : columns -> string
[<NoComparison;NoEquality>]
type row =
    | RRow of int
module Row =
    val ofNat : int -> row
    val setElemCompare : row -> row -> Ordering
type rows = OSet<row>
module Rows =
  val toString : rows -> string
[<NoComparison;NoEquality>]
type cell =
    {col: column;
     row: row;}
module Cell =
    val make : column -> row -> cell
    val setElemCompare : cell -> cell -> Ordering
    val to_string : cell -> string
type cells = OSet<cell>
module Cells =
  val toString : cells -> string
[<NoComparison;NoEquality>]
type stack =
    | SStack of int
module Stack =
    val ofNat : int -> stack
    val setElemCompare : stack -> stack -> Ordering
    val to_string : stack -> string
type stacks = OSet<stack>
module Stacks =
    val to_string : stacks -> string
type boxWidth = int
[<NoComparison;NoEquality>]
type band =
    | BBand of int
module Band =
    val ofNat : int -> band
    val setElemCompare : band -> band -> Ordering
    val to_string : band -> string
type bands = OSet<band>
module Bands =
    val to_string : bands -> string
type boxHeight = int
[<NoComparison;NoEquality>]
type bbox =
    {stack: stack;
     band: band;}
module Box =
    val make : stack -> band -> bbox
    val setElemCompare : bbox -> bbox -> Ordering
    val to_string : bbox -> string
type boxes = OSet<bbox>
module Boxes =
    val to_string : boxes -> string
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
    val setElemCompare : house -> house -> Ordering
    val to_string : house -> string
type houses = OSet<house>
module Houses =
    val toString : houses -> string
[<NoComparison;NoEquality>]
type digit =
    | Digit of char
module Digit =
    val ofNat : int -> digit
    val setElemCompare : digit -> digit -> Ordering
    val to_string : digit -> string
type digits = OSet<digit>
module Digits =
    val toString : digits -> string
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
    val to_string : value -> string
[<NoComparison;NoEquality>]
type candidate =
    {cell: cell;
     digit: digit;}
module Candidate =
    val make : cell -> digit -> candidate
    val to_string : candidate -> string
[<NoComparison;NoEquality>]
type candidateReduction =
    {cell: cell;
     candidates: digits;}
module CandidateReduction =
    val make : cell -> digits -> candidateReduction
    val to_string : candidateReduction -> string
module CandidateReductions =
    val to_string : candidateReduction list -> string
[<NoComparison;NoEquality>]
type action =
    | Load of string
    | LoadEliminate
    | Placement of value
    | Eliminate of candidate
module Action =
  val to_string : action -> string
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
