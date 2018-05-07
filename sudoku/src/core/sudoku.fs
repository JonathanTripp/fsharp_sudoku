module core.Sudoku

open compat
open compat.Sset
open compat.oset
open compat.smap

(* A sudoku is a square grid of size... *)
type size = int

(* containing columns... *)
[<NoComparison;NoEquality>]
type column = 
    | CColumn of int

module Column =
    let ofNat (i : int) : column =
        CColumn i

    let print (CColumn c : column) : string =
        Printf.sprintf "c%d" c

    let setElemCompare (CColumn lhs : column) (CColumn rhs : column) : Ordering =
        if lhs < rhs then LT
        else if lhs = rhs then EQ
        else GT

type columns = OSet<column>

module Columns =
    let print (c : columns) =
        OSet.print Column.print c

(* ... by rows *)
[<NoComparison;NoEquality>]
type row = 
    | RRow of int

module Row =
    let ofNat (i : int) : row = RRow i

    let print (RRow r : row) : string =
        Printf.sprintf "r%d" r

    let setElemCompare (RRow lhs : row) (RRow rhs : row) : Ordering =
        if lhs < rhs then LT
        else if lhs = rhs then EQ
        else GT

type rows = OSet<row>

module Rows =
    let print (r : rows) =
        OSet.print Row.print r

(* Each cell is identified by (col, row) *)
[<NoComparison;NoEquality>]
type cell = 
    { col : column;
      row : row }

module Cell =
    let make (c : column) (r : row) : cell =
        { col = c;
          row = r }

    let print ({col = CColumn c; row = RRow r} : cell) : string =
        Printf.sprintf "c%dr%d" c r

    let setElemCompare ({ col = CColumn c1; row = RRow r1} : cell) ({ col = CColumn c2; row = RRow r2} : cell) : Ordering =
        if r1 < r2 then LT
        else if r1 = r2 then
            if c1 < c2 then LT
            else if c1 = c2 then EQ
            else GT
        else GT

type cells = OSet<cell>

module Cells =
    let print (cs : cells) : string =
        "CS" + (OSet.print Cell.print cs)

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

    let print (SStack s : stack) : string =
        Printf.sprintf "stk%d" s

    let setElemCompare (SStack lhs : stack) (SStack rhs : stack) : Ordering =
        if lhs < rhs then LT
        else if lhs = rhs then EQ
        else GT

type stacks = OSet<stack>

module Stacks =
    let print (ss : stacks) : string =
        "SS" + (OSet.print Stack.print ss)

type boxWidth = int

(* A row of horizontal boxes is a band *)
[<NoComparison;NoEquality>]
type band = 
    | BBand of int

module Band =
    let ofNat (i : int) : band =
        BBand i

    let print (BBand b : band) : string =
        Printf.sprintf "bnd%d" b

    let setElemCompare (BBand lhs : band) (BBand rhs : band) : Ordering =
        if lhs < rhs then LT
        else if lhs = rhs then EQ
        else GT

type bands = OSet<band>

module Bands =
    let print (bs : bands) : string =
        "BD" + (OSet.print Band.print bs)

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

    let print ({stack = SStack s; band = BBand b} : bbox) : string =
        Printf.sprintf "bnd%dstk%d" b s

    let setElemCompare ({ stack = SStack s1; band = BBand b1} : bbox) ({ stack = SStack s2; band = BBand b2} : bbox) : Ordering =
        if b1 < b2 then LT
        else if b1 = b2 then
            if s1 < s2 then LT
            else if s1 = s2 then EQ
            else GT
        else GT

type boxes = OSet<bbox>

module Boxes =
    let print (bs : boxes) : string =
        "B" + (OSet.print Box.print bs)

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

    let print (house : house) : string =
        match house with
        | HColumn column -> Column.print column
        | HRow row -> Row.print row
        | HBox box -> Box.print box

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

type houses = OSet<house>

module Houses =
    let print (hs : houses) : string =
        "H" + (OSet.print House.print hs)

(* Each cell in the grid contains a Digit, usually numbers 1..9 *)
[<NoComparison;NoEquality>]
type digit = 
    | Digit of char

module Digit =
    let ofNat (i : int) : digit =
        Digit (Schar.chr (i + (Schar.code '0')))

    let print (Digit s : digit) : string =
        Sstring.make 1 s

    let setElemCompare (Digit lhs : digit ) (Digit rhs : digit) : Ordering =
        if lhs < rhs then LT
        else if lhs = rhs then EQ
        else GT

type digits = OSet<digit>

module Digits =
    let print (ds : digits) : string =
        "D" + (OSet.print Digit.print ds)

let registerSetElemComparers () : unit =
    SetElemComparers.Register Column.setElemCompare
    SetElemComparers.Register Row.setElemCompare
    SetElemComparers.Register Cell.setElemCompare
    SetElemComparers.Register Stack.setElemCompare
    SetElemComparers.Register Band.setElemCompare
    SetElemComparers.Register Box.setElemCompare
    SetElemComparers.Register Line.setElemCompare
    SetElemComparers.Register House.setElemCompare
    SetElemComparers.Register Digit.setElemCompare

registerSetElemComparers()

(* A sudoku is defined by the overall grid size (it is always square)
 which is the same as the OSet in the alphabet
 and also by the width and height of the boxes *)
[<NoComparison;NoEquality>]
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
 [<NoComparison;NoEquality>]
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
 [<NoComparison;NoEquality>]
type value = 
    { cell : cell;
      digit : digit }

module Value =
    let make (cell : cell) (digit : digit) : value = 
        { cell = cell;
          digit = digit }

    let print ({ cell = cell; digit = digit} : value) : string =
        Printf.sprintf "%s=%s" (Cell.print cell) (Digit.print digit)

(* A candidate is a digit in a cell, which is still a pencilmark *)
[<NoComparison;NoEquality>]
type candidate = 
    { cell : cell;
      digit : digit }

module Candidate =
    let make (cell : cell) (digit : digit) : candidate =
        { cell = cell;
          digit = digit }

    let print ({ cell = cell; digit = digit} : candidate) : string =
        Printf.sprintf "(%s)%s" (Cell.print cell) (Digit.print digit)

[<NoComparison;NoEquality>]
type candidateReduction = 
    { cell : cell;
      candidates : digits }

module CandidateReduction =
    let make (cell : cell) (digits : digits) : candidateReduction =
        { cell = cell;
          candidates = digits }

    let print ({ cell = cell; candidates = digits} : candidateReduction) : string =
        Printf.sprintf "Cell %s, Candidates %s" (Cell.print cell) (OSet.print Digit.print digits)

module CandidateReductions =
    let print (s : candidateReduction list) : string =
        s
        |> List.map CandidateReduction.print
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
    let print (action : action) : string =
        match action with
        | Load sudoku -> Printf.sprintf "Load:%s" sudoku
        | LoadEliminate  -> "Load"
        | Placement a -> Printf.sprintf "%s=%s" (Cell.print a.cell) (Digit.print a.digit)
        | Eliminate candidate -> Printf.sprintf "%s<>%s" (Cell.print candidate.cell) (Digit.print candidate.digit)

[<NoComparison;NoEquality>]
type given = SMap<cell, digit option>

[<NoComparison;NoEquality>]
type current = SMap<cell, cellContents>

(* for a cell, return a set of candidates *)
[<NoComparison;NoEquality>]
type cellCandidates = SMap<cell, digits>

[<NoComparison;NoEquality>]
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
            | BigNumber _ -> OSet.empty()
            | PencilMarks s -> s
            in

        cells
        |> SMap.ofLookup getCandidateEntries
