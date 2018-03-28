module console.Format

open core.Sudoku
open oset
open smap

type basic_color =
    | DefaultColour
    | Black
    | Red
    | Green
    | Yellow
    | Blue
    | Magenta
    | Cyan
    | White

(* Things we may want to write *)
type consoleChar = 
    | CNil
    | CChar of char
    | CStr of string
    | CDigit of digit
    | ColouredString of string * basic_color * basic_color
    | ColouredDigit of digit * basic_color * basic_color
    | NL

type consoleString = consoleChar list

(* Printing a row, we need special characters at left, in the middle and on the right *)
type gridCharsRow = 
    { l : consoleString;
      m : consoleString;
      r : consoleString }

(* Printing a grid, we need special rows at top, in the middle and on the bottom
 Also, horizontal and vertical spacers *)
type gridChars = 
    { h : consoleString;
      v : gridCharsRow;
      t : gridCharsRow;
      m : gridCharsRow;
      b : gridCharsRow;
      n : consoleString }

type candidateGridCharsRow = 
    { mi : consoleString;
      x : gridCharsRow }

type candidateGridChars = 
    { h : consoleString;
      hi : consoleString;
      v : gridCharsRow;
      vi : consoleString;
      t : candidateGridCharsRow;
      m : candidateGridCharsRow;
      mi : candidateGridCharsRow;
      b : candidateGridCharsRow;
      n : consoleString }

let konst x _ = x

let printLine (cells : cells) (digitTo : cell -> consoleString) : consoleString = 
    cells
    |> OSet.map digitTo
    |> OSet.toList
    |> List.concat

(* Combine fences with posts (there's one more fence than posts: f p f p ... p f) *)
let simpleInterleave (fenceToSeq : 'a -> consoleString) (post : consoleString) (fences : OSet<'a>) : consoleString = 
    let rec gen (fences' : 'a list) : consoleString = 
        match fences' with
        | [] -> []
        | [f] -> fenceToSeq f
        | f :: fs -> List.concat [(fenceToSeq f); post; (gen fs)]
        in
     gen (fences |> OSet.toList)

(* Create a sequence of fences interleaved with posts (first and last posts may be different)
 l f p f p f ... p f r *)
let sinterleave (fenceToSeq : 'a -> consoleString) (firstPost : consoleString) (midPost : consoleString) (lastPost : consoleString) (eol : consoleString) (fences : OSet<'a>) : consoleString = 
    List.concat [firstPost; simpleInterleave fenceToSeq midPost fences; lastPost; eol]

(* Print a column *)
let printCell (digitTo : cell -> consoleString) (cell : cell) : consoleString = 
    digitTo cell

let printColumn (printCell : cell -> consoleString) (row : row) (column : column) : consoleString = 
    let cell = Cell.make column row in
    printCell cell

(* Print a stack *)
let printStack (p : core.Puzzlemap.puzzleMap) (columnPrinter : row -> column -> consoleString) (columnSeparator : consoleString) (row : row) (stack : stack) : consoleString = 
    simpleInterleave (columnPrinter row) columnSeparator (SMap.get stack p.stackColumns)

(* Print a row *)
let printRow (stackPrinter : stack -> consoleString) (gridCharsRow : gridCharsRow) (eol : consoleString) (stacks : stacks) : consoleString = 
    List.concat [gridCharsRow.l; simpleInterleave stackPrinter gridCharsRow.m stacks; gridCharsRow.r; eol ]

(* Print a band *)
let printBand (p : core.Puzzlemap.puzzleMap) (rowToSeq : row -> consoleString) (rowSeparator : consoleString) (band : band) : consoleString = 
    simpleInterleave rowToSeq rowSeparator (SMap.get band p.bandRows)

(* Print a puzzle grid, supply callback to draw each cell *)
let printGrid (p : core.Puzzlemap.puzzleMap) (gridChars : gridChars) (digitTo : cell -> consoleString) : consoleString = 

    let doPrintColumn : row -> column -> consoleString = printColumn (printCell digitTo) in

    let doPrintStack : row -> stack -> consoleString = printStack p doPrintColumn [] in

    let doPrintRow : row -> consoleString = fun row -> printRow (doPrintStack row) gridChars.v gridChars.n p.stacks in

    let doPrintBand : band -> consoleString = printBand p doPrintRow [] in

    let r : consoleString =
        (SMap.get (OSet.head p.stacks) p.stackColumns)
        |> OSet.toList
        |> List.map (konst gridChars.h)
        |> List.concat
        in

    let printHorizontal (g : gridCharsRow) : consoleString = sinterleave (konst r) g.l g.m g.r gridChars.n p.stacks in

    let t = printHorizontal gridChars.t in
    let m = printHorizontal gridChars.m in
    let b = printHorizontal gridChars.b in

    sinterleave doPrintBand t m b [] p.bands

let printCandidateGrid (p : core.Puzzlemap.puzzleMap) (candidateGridChars : candidateGridChars) (alphabet : digits) (draw_cell : cell -> digit -> consoleString) : consoleString = 

    let d : consoleString =
        SMap.get (OSet.head p.stacks) p.stackColumns
        |> OSet.map (konst candidateGridChars.h)
        |> OSet.toList
        |> List.concat
        in

    let i : consoleString =
        SMap.get (OSet.head p.stacks) p.stackColumns
        |> OSet.map (konst candidateGridChars.hi)
        |> OSet.toList
        |> List.concat
        in

    let printFullHorizontal (x : candidateGridCharsRow) (i : consoleString) : consoleString = 
        let s = simpleInterleave (konst i) x.mi (SMap.get (OSet.head p.stacks) p.stackColumns) in

        sinterleave (konst s) x.x.l x.x.m x.x.r candidateGridChars.n p.stacks
        in

    let c : int = OSet.count (SMap.get (OSet.head p.stacks) p.stackColumns) in
    
    let ss : OSet<digits> = 
        Sset.srange 0 (OSet.count p.stacks - 1)
        |> OSet.map (fun i -> OSet.skip (i * c) alphabet |> OSet.take c)
        in

    let doPrintColumn (digits : digits) : row -> column -> consoleString = 
        let doPrintCell : cell -> consoleString =
            fun cell ->
                digits
                |> OSet.map (fun digit -> draw_cell cell digit)
                |> OSet.toList
                |> List.concat
                in
        printColumn doPrintCell
        in

    let doPrintStack (digits : digits) : row -> stack -> consoleString =
        printStack p (doPrintColumn digits) candidateGridChars.vi
        in

    let doPrintRow (row : row) : consoleString = 
        ss
        |> OSet.map
            (fun digits -> printRow (doPrintStack digits row) candidateGridChars.v candidateGridChars.n p.stacks)
        |> OSet.toList
        |> List.concat
        in

    let t : consoleString = printFullHorizontal candidateGridChars.t d in
    let m : consoleString = printFullHorizontal candidateGridChars.m d in
    let b : consoleString = printFullHorizontal candidateGridChars.b d in

    let rowSeparator : consoleString = printFullHorizontal candidateGridChars.mi i in

    let doPrintBand : band -> consoleString = printBand p doPrintRow rowSeparator in

    sinterleave doPrintBand t m b [] p.bands
