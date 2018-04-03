module core.Sudoku

open oset
open smap

type size = int
type column =
    | CColumn of int
    interface OSetMember<column>
module Column =
    val ofNat : int -> column
type columns = OSet<column>
type row =
    | RRow of int
    interface OSetMember<row>
module Row =
    val ofNat : int -> row
type rows = OSet<row>
type cell =
    {col: column;
     row: row;}
    interface OSetMember<cell>
module Cell =
    val make : column -> row -> cell
    val to_string : cell -> string
type cells = OSet<cell>
module Cells =
  val toString : cells -> string
type stack =
    | SStack of int
    interface OSetMember<stack>
module Stack =
    val ofNat : int -> stack
    val to_string : stack -> string
type stacks = OSet<stack>
module Stacks =
    val to_string : stacks -> string
type boxWidth = int
type band =
    | BBand of int
    interface OSetMember<band>
module Band =
    val ofNat : int -> band
    val to_string : band -> string
type bands = OSet<band>
module Bands =
    val to_string : bands -> string
type boxHeight = int
type box =
    {stack: stack;
     band: band;}
    interface OSetMember<box>
module Box =
    val make : stack -> band -> box
    val to_string : box -> string
type boxes = OSet<box>
module Boxes =
    val to_string : boxes -> string
type line =
    | LColumn of column
    | LRow of row
    interface OSetMember<line>
type house =
    | HColumn of column
    | HRow of row
    | HBox of box
    interface OSetMember<house>
module House =
    val make_column : column -> house
    val make_row : row -> house
    val make_box : box -> house
    val to_string : house -> string
type houses = OSet<house>
module Houses =
    val toString : houses -> string
type digit =
    | Digit of char
    interface OSetMember<digit>
module Digit =
    val ofNat : int -> digit
    val to_string : digit -> string
type digits = OSet<digit>
module Digits =
    val toString : digits -> string
type puzzleShape =
    {size: size;
     boxWidth: boxWidth;
     boxHeight: boxHeight;
     alphabet: digits;}
module PuzzleShape =
    val default' : puzzleShape
type cellContents =
    | BigNumber of digit
    | PencilMarks of digits
    interface OSetMember<cellContents>
module CellContents =
    val make_big_number : digit -> cellContents
    val make_pencil_marks : digits -> cellContents
type value =
    {cell: cell;
     digit: digit;}
module Value =
    val make : cell -> digit -> value
    val to_string : value -> string
type candidate =
    {cell: cell;
     digit: digit;}
module Candidate =
    val make : cell -> digit -> candidate
    val to_string : candidate -> string
type candidateReduction =
    {cell: cell;
     candidates: digits;}
    interface OSetMember<candidateReduction>
module CandidateReduction =
    val make : cell -> digits -> candidateReduction
    val to_string : candidateReduction -> string
module CandidateReductions =
    val to_string : candidateReduction list -> string
type action =
    | Load of string
    | LoadEliminate
    | Placement of value
    | Eliminate of candidate
module Action =
  val to_string : action -> string
type given = SMap<cell, digit option>
type current = SMap<cell, cellContents>
type cellCandidates = SMap<cell, digits>
type solution =
    {given: given;
     current: current;
     steps: action list;}
module Solution =
    val givenToCurrent : cells -> given -> digits -> current
    val currentCellCandidates : cells -> current -> cellCandidates
