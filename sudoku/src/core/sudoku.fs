module core.Sudoku

open oset
open smap

(* A sudoku is a square grid of size... *)
type size = int

(* containing columns... *)
type column = 
    | CColumn of int

module Column =
    let ofNat (i : int) : column =
        CColumn i

    let to_string (CColumn c : column) : string =
        Printf.sprintf "c%d" c

type columns = OSet<column>

module Columns =
    let toString (s : columns) : string =
        "C" + (OSet.toString s)

(* ... by rows *)
type row = 
    | RRow of int

module Row =
    let ofNat (i : int) : row = RRow i

    let to_string (RRow r : row) : string =
        Printf.sprintf "r%d" r

type rows = OSet<row>

module Rows =
    let toString (r : rows) : string =
        "R" + (OSet.toString r)

(* Each cell is identified by (col, row) *)
type cell = 
    { col : column;
      row : row }

module Cell =
    let make (c : column) (r : row) : cell =
        { col = c;
          row = r }

    let to_string ({col = CColumn c; row = RRow r} : cell) : string =
        Printf.sprintf "r%dc%d" r c

type cells = OSet<cell>

module Cells =
    let toString (cs : cells) : string =
        "CS" + (OSet.toString cs)

(* The grid is divided into boxes,
 these do not have to be square, but they are
 all the same size and cover the grid
 A column of vertical boxes is a stack *)
type stack = 
    | SStack of int

module Stack =
    let ofNat (i : int) : stack =
        SStack i

    let to_string (SStack s : stack) : string =
        Printf.sprintf "stk%d" s

type stacks = OSet<stack>

module Stacks =
    let to_string (ss : OSet<stack>) : string =
        "SS" + (OSet.toString ss)

type boxWidth = int

(* A row of horizontal boxes is a band *)
type band = 
    | BBand of int

module Band =
    let ofNat (i : int) : band =
        BBand i

    let to_string (BBand b : band) : string =
        Printf.sprintf "bnd%d" b

type bands = OSet<band>

module Bands =
    let to_string (bs : bands) : string =
        "BD" + (OSet.toString bs)

type boxHeight = int

(* A box is the intersection of a stack and a band *)
type box = 
    { stack : stack;
      band : band }

module Box =
    let make (s : stack) (b : band) : box =
        { stack = s;
          band = b }

    let to_string ({stack = SStack s; band = BBand b} : box) : string =
        Printf.sprintf "bnd%dstk%d" b s

type boxes = OSet<box>

module Boxes =
    let to_string (bs : boxes) : string =
        "B" + (OSet.toString bs)

(* The columns and rows are collectively called lines *)
type line = 
    | LColumn of column
    | LRow of row

(* The columns, rows and boxes are collectively called houses *)
type house = 
    | HColumn of column
    | HRow of row
    | HBox of box

module House =
    let make_column (column : column) : house =
        HColumn column

    let make_row (row : row) : house =
        HRow row

    let make_box (box : box) : house =
        HBox box

    let to_string (house : house) : string =
        match house with
        | HColumn column -> Column.to_string column
        | HRow row -> Row.to_string row
        | HBox box -> Box.to_string box

type houses = OSet<house>

module Houses =
    let toString (hs : houses) : string =
        "H" + (OSet.toString hs)

(* Each cell in the grid contains a Digit, usually numbers 1..9 *)
type digit = 
    | Digit of char

module Digit =
    let ofNat (i : int) : digit =
        Digit (Schar.chr (i + (Schar.code '0')))

    let to_string (Digit s : digit) : string =
        Sstring.make 1 s

type digits = OSet<digit>

module Digits =
    let toString (ds : digits) : string =
        "D" + (OSet.toString ds)

(* A sudoku is defined by the overall grid size (it is always square)
 which is the same as the OSet in the alphabet
 and also by the width and height of the boxes *)
type puzzleShape = 
    { size : size;
      boxWidth : boxWidth;
      boxHeight : boxHeight;
      alphabet : digits }

module PuzzleShape =
    let default' : puzzleShape = 
        { size = 9;
          boxWidth = 3;
          boxHeight = 3;
          alphabet = OSet.range 1 9 Digit.ofNat }

(* Whilst working to a solution each cell in the grid
 that doesn't have a Digit is filled with candidates
 Candidates are possible OSet *)
type cellContents = 
    | BigNumber of digit
    | PencilMarks of digits

module CellContents =
    let make_big_number (digit : digit) : cellContents =
        BigNumber digit

    let make_pencil_marks (digits : digits) : cellContents =
        PencilMarks digits

(* Working towards a solution we take one of the following actions:
 Set the cell to have a Digit *)
type value = 
    { cell : cell;
      digit : digit }

module Value =
    let make (cell : cell) (digit : digit) : value = 
        { cell = cell;
          digit = digit }

    let to_string ({ cell = cell; digit = digit} : value) : string =
        Printf.sprintf "%s=%s" (Cell.to_string cell) (Digit.to_string digit)

(* A candidate is a digit in a cell, which is still a pencilmark *)
type candidate = 
    { cell : cell;
      digit : digit }

module Candidate =
    let make (cell : cell) (digit : digit) : candidate =
        { cell = cell;
          digit = digit }

    let to_string ({ cell = cell; digit = digit} : candidate) : string =
        Printf.sprintf "(%s)%s" (Cell.to_string cell) (Digit.to_string digit)

type candidateReduction = 
    { cell : cell;
      candidates : digits }

module CandidateReduction =
    let make (cell : cell) (digits : digits) : candidateReduction =
        { cell = cell;
          candidates = digits }

    let to_string ({ cell = cell; candidates = digits} : candidateReduction) : string =
        Printf.sprintf "Cell %s, Candidates %s" (Cell.to_string cell) (OSet.toString digits)

module CandidateReductions =
    let to_string (s : candidateReduction list) : string =
        s
        |> List.map CandidateReduction.to_string
        |> String.concat ","

(* Working towards a solution we take one of the following actions:
 Set the cell to have a Digit
 or remove a candidate *)
type action =
    | Load of string
    | LoadEliminate
    | Placement of value
    | Eliminate of candidate

module Action =
    let to_string (action : action) : string =
        match action with
        | Load sudoku -> Printf.sprintf "Load:%s" sudoku
        | LoadEliminate  -> "Load"
        | Placement a -> Printf.sprintf "%s=%s" (Cell.to_string a.cell) (Digit.to_string a.digit)
        | Eliminate candidate -> Printf.sprintf "%s<>%s" (Cell.to_string candidate.cell) (Digit.to_string candidate.digit)

type given = SMap<cell, digit option>

type current = SMap<cell, cellContents>

(* for a cell, return a set of candidates *)
type cellCandidates = SMap<cell, digits>

type solution = 
    { given : given;
      current : current;
      steps : action list }

module Solution =
    let givenToCurrent (cells : cells) (given : given) (alphabet : digits) : current =
        let makeCellContents (cell : cell) : cellContents =
            let dop = SMap.get cell given in
            match dop with
            | Some digit -> BigNumber digit
            | None -> PencilMarks alphabet
            in

        cells
        |> SMap.ofLookup makeCellContents

    let currentCellCandidates (cells : cells) (current : current) : cellCandidates =
        let getCandidateEntries (cell : cell) : digits =
            let cellContents = SMap.get cell current in
            match cellContents with
            | BigNumber _ -> OSet.empty
            | PencilMarks s -> s
            in

        cells
        |> SMap.ofLookup getCandidateEntries
