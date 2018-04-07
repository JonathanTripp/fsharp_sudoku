module core.Sudoku

open Sset
open oset
open smap

(* A sudoku is a square grid of size... *)
type size = int

(* containing columns... *)
type column = 
    | CColumn of int
    with
    static member setElemCompare (CColumn lhs:column) (CColumn rhs:column) : Ordering =
        if lhs < rhs then LT
        else if lhs = rhs then EQ
        else GT

    override this.ToString() =
        let (CColumn c) = this in
        Printf.sprintf "c%d" c

module Column =
    let ofNat (i : int) : column =
        CColumn i

type columns = OSet<column>

module Columns =
    let toString (c : columns) = c.ToString()

(* ... by rows *)
type row = 
    | RRow of int
    static member setElemCompare (RRow lhs:row) (RRow rhs:row) : Ordering =
        if lhs < rhs then LT
        else if lhs = rhs then EQ
        else GT

    override this.ToString() =
        let (RRow r) = this in
        Printf.sprintf "r%d" r

module Row =
    let ofNat (i : int) : row = RRow i

type rows = OSet<row>

module Rows =
    let toString (r : rows) = r.ToString()

(* Each cell is identified by (col, row) *)
type cell = 
    { col : column;
      row : row }
    static member setElemCompare ({ col = CColumn c1; row = RRow r1} : cell) ({ col = CColumn c2; row = RRow r2} : cell) : Ordering =
        if r1 < r2 then LT
        else if r1 = r2 then
            if c1 < c2 then LT
            else if c1 = c2 then EQ
            else GT
        else GT

    override this.ToString() =
        let {col = CColumn c; row = RRow r} = this in
        Printf.sprintf "c%dr%d" c r

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
    static member setElemCompare (SStack lhs:stack) (SStack rhs:stack) : Ordering =
        if lhs < rhs then LT
        else if lhs = rhs then EQ
        else GT

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
    static member setElemCompare (BBand lhs:band) (BBand rhs:band) : Ordering =
        if lhs < rhs then LT
        else if lhs = rhs then EQ
        else GT

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
type bbox = 
    { stack : stack;
      band : band }
    static member setElemCompare ({ stack = SStack s1; band = BBand b1} : bbox) ({ stack = SStack s2; band = BBand b2} : bbox) : Ordering =
        if b1 < b2 then LT
        else if b1 = b2 then
            if s1 < s2 then LT
            else if s1 = s2 then EQ
            else GT
        else GT

module Box =
    let make (s : stack) (b : band) : bbox =
        { stack = s;
          band = b }

    let to_string ({stack = SStack s; band = BBand b} : bbox) : string =
        Printf.sprintf "bnd%dstk%d" b s

type boxes = OSet<bbox>

module Boxes =
    let to_string (bs : boxes) : string =
        "B" + (OSet.toString bs)

(* The columns and rows are collectively called lines *)
type line = 
    | LColumn of column
    | LRow of row
    static member setElemCompare (l1 : line) (l2 : line) : Ordering =
        match l1, l2 with
        | LColumn c1, LColumn c2 -> column.setElemCompare c1 c2
        | LColumn _, LRow _ -> LT
        | LRow _, LColumn _ -> GT
        | LRow r1, LRow r2 -> row.setElemCompare r1 r2

(* The columns, rows and boxes are collectively called houses *)
type house = 
    | HColumn of column
    | HRow of row
    | HBox of bbox
    static member setElemCompare (h1 : house) (h2 : house) : Ordering =
        match h1, h2 with
        | HColumn c1, HColumn c2 -> column.setElemCompare c1 c2
        | HColumn _, HRow _ -> LT
        | HColumn _, HBox _ -> LT
        | HRow _, HColumn _ -> GT
        | HRow r1, HRow r2 -> row.setElemCompare r1 r2
        | HRow _, HBox _ -> LT
        | HBox _, HColumn _ -> GT
        | HBox _, HRow _ -> GT
        | HBox b1, HBox b2 -> bbox.setElemCompare b1 b2

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

type houses = OSet<house>

module Houses =
    let toString (hs : houses) : string =
        "H" + (OSet.toString hs)

(* Each cell in the grid contains a Digit, usually numbers 1..9 *)
type digit = 
    | Digit of char
    static member setElemCompare (Digit lhs:digit) (Digit rhs:digit) : Ordering =
        if lhs < rhs then LT
        else if lhs = rhs then EQ
        else GT

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
