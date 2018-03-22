module core.Sudoku

open oset

type size = int
type column = | CColumn of int
module Column =
  val comparer : column -> column -> int
  val make : int -> column
  val to_string : column -> string
type columns = OSet<column>
module Columns =
  val toString : columns -> string
type row = | RRow of int
module Row =
  val comparer : row -> row -> int
  val make : int -> row
  val to_string : row -> string
type rows = OSet<row>
module Rows =
  val toString : rows -> string
type cell =
  {col: column;
   row: row;}
module Cell =
  val comparer : cell -> cell -> int
  val make : column -> row -> cell
  val to_string : cell -> string
type cells = OSet<cell>
module Cells =
  val ofLookup : (cell -> 'b) -> cells -> (cell * 'b) list
  val toString : cells -> string
type stack = | SStack of int
module Stack =
  val comparer : stack -> stack -> int
  val make : int -> stack
  val to_string : stack -> string
module Stacks =
  val to_string : stack list -> string
type boxWidth = int
type band = | BBand of int
module Band =
  val comparer : band -> band -> int
  val make : int -> band
  val to_string : band -> string
module Bands =
  val to_string : band list -> string
type boxHeight = int
type box =
  {stack: stack;
   band: band;}
module Box =
  val comparer : box -> box -> int
  val make : stack -> band -> box
  val to_string : box -> string
module Boxes =
  val to_string : box list -> string
type line =
  | LColumn of column
  | LRow of row
type house =
  | HColumn of column
  | HRow of row
  | HBox of box
module House =
  val comparer : house -> house -> int
  val make_column : column -> house
  val make_row : row -> house
  val make_box : box -> house
  val to_string : house -> string
type houses = OSet<house>
module Houses2 =
  val toString : houses -> string
type digit = | Digit of char
module Digit =
  val comparer : digit -> digit -> int
  val make : int -> digit
  val to_string : digit -> string
type digits = | CDigits of digit list
module Digits =
  val contains : digit -> digits -> bool
  val count : digits -> int
  val difference : digits -> digits -> digits
  val drop : int -> digits -> digits
  val empty : digits
  val filter : (digit -> bool) -> digits -> digits
  val first : digits -> digit
  val intersect : digits -> digits -> digits
  val is_subset : digits -> digits -> bool
  val make : digit list -> digits
  val map : (digit -> 'b) -> digits -> 'b list
  val nth : digits -> int -> digit
  val remove : digit -> digits -> digits
  val singleton : digit -> digits
  val take : int -> digits -> digits
  val to_list : digits -> digit list
  val union : digits -> digits -> digits
  val union_many : digits list -> digits
  val to_string : digits -> string
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
type given = Given of (cell * digit option) list
module Given =
    val get : cell -> given -> digit option
type current = Current of (cell * cellContents) list
module Current =
  val get : cell -> current -> cellContents
  val make : (cell * cellContents) list -> current
type cellCandidates = CellCandidates of (cell * digits) list
module CellCandidates =
    val get : cell -> cellCandidates -> digits
type solution =
  {given: given;
   current: current;
   steps: action list;}
module Solution =
  val givenToCurrent : cells -> given -> digits -> current
  val currentCellCandidates : cells -> current -> cellCandidates
