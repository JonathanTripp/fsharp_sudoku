module core.Sudoku

open Sset
open oset
open smap

(* A sudoku is a square grid of size... *)
type size = int

(* containing columns... *)
[<NoComparison;NoEquality>]
type column = 
    | CColumn of int
    override this.ToString() =
        let (CColumn c) = this in
        Printf.sprintf "c%d" c

module Column =
    let ofNat (i : int) : column =
        CColumn i

    let setElemCompare (CColumn lhs : column) (CColumn rhs : column) : Ordering =
        if lhs < rhs then LT
        else if lhs = rhs then EQ
        else GT

SetElemComparers.Register Column.setElemCompare

type columns = OSet<column>

module Columns =
    let toString (c : columns) = c.ToString()

(* ... by rows *)
[<NoComparison;NoEquality>]
type row = 
    | RRow of int
    override this.ToString() =
        let (RRow r) = this in
        Printf.sprintf "r%d" r

module Row =
    let ofNat (i : int) : row = RRow i

    let setElemCompare (RRow lhs : row) (RRow rhs : row) : Ordering =
        if lhs < rhs then LT
        else if lhs = rhs then EQ
        else GT

SetElemComparers.Register Row.setElemCompare

type rows = OSet<row>

module Rows =
    let toString (r : rows) = r.ToString()

(* Each cell is identified by (col, row) *)
[<NoComparison;NoEquality>]
type cell = 
    { col : column;
      row : row }
    override this.ToString() =
        let {col = CColumn c; row = RRow r} = this in
        Printf.sprintf "c%dr%d" c r

module Cell =
    let make (c : column) (r : row) : cell =
        { col = c;
          row = r }

    let to_string ({col = CColumn c; row = RRow r} : cell) : string =
        Printf.sprintf "r%dc%d" r c

    let setElemCompare ({ col = CColumn c1; row = RRow r1} : cell) ({ col = CColumn c2; row = RRow r2} : cell) : Ordering =
        if r1 < r2 then LT
        else if r1 = r2 then
            if c1 < c2 then LT
            else if c1 = c2 then EQ
            else GT
        else GT

SetElemComparers.Register Cell.setElemCompare

type cells = OSet<cell>

module Cells =
    let toString (cs : cells) : string =
        "CS" + (OSet.toString Cell.to_string cs)

(* The grid is divided into boxes,
 these do not have to be square, but they are
 all the same size and cover the grid
 A column of vertical boxes is a stack *)
 [<NoComparison;NoEquality>]
type stack = 
    | SStack of int

module Stack =
    let ofNat (i : int) : stack =
        SStack i

    let to_string (SStack s : stack) : string =
        Printf.sprintf "stk%d" s

    let setElemCompare (SStack lhs : stack) (SStack rhs : stack) : Ordering =
        if lhs < rhs then LT
        else if lhs = rhs then EQ
        else GT

SetElemComparers.Register Stack.setElemCompare

type stacks = OSet<stack>

module Stacks =
    let to_string (ss : OSet<stack>) : string =
        "SS" + (OSet.toString Stack.to_string ss)

type boxWidth = int

(* A row of horizontal boxes is a band *)
[<NoComparison;NoEquality>]
type band = 
    | BBand of int

module Band =
    let ofNat (i : int) : band =
        BBand i

    let to_string (BBand b : band) : string =
        Printf.sprintf "bnd%d" b

    let setElemCompare (BBand lhs : band) (BBand rhs : band) : Ordering =
        if lhs < rhs then LT
        else if lhs = rhs then EQ
        else GT

SetElemComparers.Register Band.setElemCompare

type bands = OSet<band>

module Bands =
    let to_string (bs : bands) : string =
        "BD" + (OSet.toString Band.to_string bs)

type boxHeight = int

(* A box is the intersection of a stack and a band *)
[<NoComparison;NoEquality>]
type bbox = 
    { stack : stack;
      band : band }

module Box =
    let make (s : stack) (b : band) : bbox =
        { stack = s;
          band = b }

    let to_string ({stack = SStack s; band = BBand b} : bbox) : string =
        Printf.sprintf "bnd%dstk%d" b s

    let setElemCompare ({ stack = SStack s1; band = BBand b1} : bbox) ({ stack = SStack s2; band = BBand b2} : bbox) : Ordering =
        if b1 < b2 then LT
        else if b1 = b2 then
            if s1 < s2 then LT
            else if s1 = s2 then EQ
            else GT
        else GT

SetElemComparers.Register Box.setElemCompare

type boxes = OSet<bbox>

module Boxes =
    let to_string (bs : boxes) : string =
        "B" + (OSet.toString Box.to_string bs)

(* The columns and rows are collectively called lines *)
[<NoComparison;NoEquality>]
type line = 
    | LColumn of column
    | LRow of row

module Line =
    let setElemCompare (l1 : line) (l2 : line) : Ordering =
        match l1, l2 with
        | LColumn c1, LColumn c2 -> Column.setElemCompare c1 c2
        | LColumn _, LRow _ -> LT
        | LRow _, LColumn _ -> GT
        | LRow r1, LRow r2 -> Row.setElemCompare r1 r2

SetElemComparers.Register Line.setElemCompare

(* The columns, rows and boxes are collectively called houses *)
[<NoComparison;NoEquality>]
type house = 
    | HColumn of column
    | HRow of row
    | HBox of bbox

module House =
    let make_column (column : column) : house =
        HColumn column

    let make_row (row : row) : house =
        HRow row

    let make_box (box : bbox) : house =
        HBox box

    let to_string (house : house) : string =
        match house with
        | HColumn column -> column.ToString()
        | HRow row -> row.ToString()
        | HBox box -> Box.to_string box

    let setElemCompare (h1 : house) (h2 : house) : Ordering =
        match h1, h2 with
        | HColumn c1, HColumn c2 -> Column.setElemCompare c1 c2
        | HColumn _, HRow _ -> LT
        | HColumn _, HBox _ -> LT
        | HRow _, HColumn _ -> GT
        | HRow r1, HRow r2 -> Row.setElemCompare r1 r2
        | HRow _, HBox _ -> LT
        | HBox _, HColumn _ -> GT
        | HBox _, HRow _ -> GT
        | HBox b1, HBox b2 -> Box.setElemCompare b1 b2

SetElemComparers.Register House.setElemCompare

type houses = OSet<house>

module Houses =
    let toString (hs : houses) : string =
        "H" + (OSet.toString House.to_string hs)

(* Each cell in the grid contains a Digit, usually numbers 1..9 *)
[<NoComparison;NoEquality>]
type digit = 
    | Digit of char

module Digit =
    let ofNat (i : int) : digit =
        Digit (Schar.chr (i + (Schar.code '0')))

    let to_string (Digit s : digit) : string =
        Sstring.make 1 s

    let setElemCompare (Digit lhs : digit ) (Digit rhs : digit) : Ordering =
        if lhs < rhs then LT
        else if lhs = rhs then EQ
        else GT

SetElemComparers.Register Digit.setElemCompare

type digits = OSet<digit>

module Digits =
    let toString (ds : OSet<digit>) : string =
        "D" + (OSet.toString Digit.to_string ds)

(* A sudoku is defined by the overall grid size (it is always square)
 which is the same as the OSet in the alphabet
 and also by the width and height of the boxes *)
 [<NoComparison;NoEquality>]
type puzzleShape = 
    { size : size;
      boxWidth : boxWidth;
      boxHeight : boxHeight;
      alphabet : OSet<digit> }

module PuzzleShape =
    let default' : puzzleShape = 
        { size = 9;
          boxWidth = 3;
          boxHeight = 3;
          alphabet = OSet.range 1 9 Digit.ofNat }

(* Whilst working to a solution each cell in the grid
 that doesn't have a Digit is filled with candidates
 Candidates are possible OSet *)
 [<NoComparison;NoEquality>]
type cellContents = 
    | BigNumber of digit
    | PencilMarks of OSet<digit>

module CellContents =
    let make_big_number (digit : digit) : cellContents =
        BigNumber digit

    let make_pencil_marks (digits : OSet<digit>) : cellContents =
        PencilMarks digits

(* Working towards a solution we take one of the following actions:
 Set the cell to have a Digit *)
 [<NoComparison;NoEquality>]
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
[<NoComparison;NoEquality>]
type candidate = 
    { cell : cell;
      digit : digit }

module Candidate =
    let make (cell : cell) (digit : digit) : candidate =
        { cell = cell;
          digit = digit }

    let to_string ({ cell = cell; digit = digit} : candidate) : string =
        Printf.sprintf "(%s)%s" (Cell.to_string cell) (Digit.to_string digit)

[<NoComparison;NoEquality>]
type candidateReduction = 
    { cell : cell;
      candidates : OSet<digit> }

module CandidateReduction =
    let make (cell : cell) (digits : OSet<digit>) : candidateReduction =
        { cell = cell;
          candidates = digits }

    let to_string ({ cell = cell; candidates = digits} : candidateReduction) : string =
        Printf.sprintf "Cell %s, Candidates %s" (Cell.to_string cell) (OSet.toString Digit.to_string digits)

module CandidateReductions =
    let to_string (s : candidateReduction list) : string =
        s
        |> List.map CandidateReduction.to_string
        |> String.concat ","

(* Working towards a solution we take one of the following actions:
 Set the cell to have a Digit
 or remove a candidate *)
 [<NoComparison;NoEquality>]
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

[<NoComparison;NoEquality>]
type given = SMap<cell, digit option>

[<NoComparison;NoEquality>]
type current = SMap<cell, cellContents>

(* for a cell, return a set of candidates *)
[<NoComparison;NoEquality>]
type cellCandidates = SMap<cell, OSet<digit>>

[<NoComparison;NoEquality>]
type solution = 
    { given : given;
      current : current;
      steps : action list }

module Solution =
    let givenToCurrent (cells : cells) (given : given) (alphabet : OSet<digit>) : current =
        let makeCellContents (cell : cell) : cellContents =
            let dop = SMap.get cell given in
            match dop with
            | Some digit -> BigNumber digit
            | None -> PencilMarks alphabet
            in

        cells
        |> SMap.ofLookup makeCellContents

    let currentCellCandidates (cells : cells) (current : current) : cellCandidates =
        let getCandidateEntries (cell : cell) : OSet<digit> =
            let cellContents = SMap.get cell current in
            match cellContents with
            | BigNumber _ -> OSet.empty()
            | PencilMarks s -> s
            in

        cells
        |> SMap.ofLookup getCandidateEntries
