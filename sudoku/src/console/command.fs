module console.Command

open compat
open compat.Sset
open compat.oset
open compat.smap
open core.Sudoku

type parse_column_or_row_results =
    | CROk of int
    | CRError of string * int

let parse_column_or_row_results_to_string (r : parse_column_or_row_results) : string =
    match r with
    | CROk i -> Printf.sprintf "%d" i
    | CRError (what, gridSize) -> Printf.sprintf "%s must be a number between 1 and %d" what gridSize

(* find a column or row *)
let parseColumnRow what gridSize term : parse_column_or_row_results =
    try
        let i =
            term |> int in
        if i >= 1 && i <= gridSize then CROk i
        else CRError (what, gridSize)
    with
        | Failure e -> CRError (what, gridSize)

[<NoComparison;NoEquality>]
type parse_cell_results = 
    | COk of cell
    | CColError of parse_column_or_row_results * int
    | CRowError of int * parse_column_or_row_results
    | CColRowError of parse_column_or_row_results * parse_column_or_row_results

let parse_cell_results_to_string (r : parse_cell_results) : string =
    match r with
    | COk cell -> Cell.print cell
    | CColError (parsedCol, row) -> Printf.sprintf "(%s,%d) column wrong, is not a cell" (parse_column_or_row_results_to_string parsedCol) row
    | CRowError (col, parsedRow) -> Printf.sprintf "(%d,%s) row wrong, is not a cell" col (parse_column_or_row_results_to_string parsedRow)
    | CColRowError (parsedCol, parsedRow) -> Printf.sprintf "(%s,%s) column and row wrong, is not a cell" (parse_column_or_row_results_to_string parsedCol) (parse_column_or_row_results_to_string parsedRow)

(* find a cell from a pair of strings *)
let parseCell (gridSize : int) (cells : cells) (termColumn : string) (termRow : string) : parse_cell_results =
    let parsedCol = parseColumnRow "Column" gridSize termColumn in
    let parsedRow = parseColumnRow "Row" gridSize termRow in

    match (parsedCol, parsedRow) with
    | (CROk col, CROk row) ->
        let cell =
            cells
            |> OSet.find (fun cell -> Column.setElemCompare cell.col (Column.ofNat col) = EQ && Row.setElemCompare cell.row (Row.ofNat row) = EQ)
            in
        COk cell
    | (CRError _, CROk row) -> CColError (parsedCol, row)
    | (CROk col, CRError _) -> CRowError (col, parsedRow)
    | (CRError _, CRError _) -> CColRowError (parsedCol, parsedRow)

let charToCandidate (digits : digits) (trialDigit : char) : digit option = 
    let compareAlpha (Digit charDigit) = trialDigit = charDigit in
    if OSet.exists compareAlpha digits then
        Some (OSet.find compareAlpha digits)
    else None

[<NoComparison;NoEquality>]
type parse_value_result =
    | VOk of digit
    | VErrorInvalid of string * string
    | VErrorTooMany of string

let parse_value_result_to_string (r : parse_value_result) : string =
    match r with
    | VOk d -> Printf.sprintf "%s" (Digit.print d)
    | VErrorInvalid (term, digits) -> Printf.sprintf "%s must be one of %s" term digits
    | VErrorTooMany term -> Printf.sprintf "%s should be single character" term

(* find an element of the alphabet *)
let parseValue (digits : digits) (term : string) : parse_value_result = 
    if String.length term = 1 then
        match charToCandidate digits (Sstring.get term 0) with
        | Some d -> VOk d
        | None -> VErrorInvalid (term, (Digits.print digits))
    else VErrorTooMany term

[<NoComparison;NoEquality>]
type focus_command_result =
    | FCOk of parse_value_result
    | FCWrongTermCount of int

let focusCommandParse (s: puzzleShape) (item : string) : focus_command_result =
    let terms = Sset.split_char ' ' item in
    if List.length terms = 2 then 
        FCOk (parseValue s.alphabet (List.item 1 terms))
    else
        FCWrongTermCount (List.length terms)

let focusCommandHintDescription (p : core.Puzzlemap.puzzleMap) (digit : digit) : core.Hint.description =
    { primaryHouses = OSet.empty();
      secondaryHouses = OSet.empty();
      candidateReductions = [];
      setCellValueAction = None;
      pointers = [];
      focus = OSet.singleton digit }

[<NoComparison;NoEquality>]
type set_cell_command_parse_result =
    | SCCOk of value
    | SCCBadParams of parse_cell_results * parse_value_result
    | SCCWrongTermCount of int

let setCellCommandParse (s: puzzleShape) (item : string) (p : core.Puzzlemap.puzzleMap) : set_cell_command_parse_result = 
    let terms = Sset.split_char ' ' item in
    if List.length terms = 4 then 
        let parsedCell = parseCell (OSet.count s.alphabet) p.cells (List.item 1 terms) (List.item 2 terms) in
        let parsedValue = parseValue s.alphabet (List.item 3 terms) in

        match (parsedCell, parsedValue) with
        | (COk cell, VOk value) -> SCCOk (Value.make cell value)
        | _ -> SCCBadParams (parsedCell, parsedValue)
    else SCCWrongTermCount (List.length terms)

[<NoComparison;NoEquality>]
type set_cell_command_check_result =
    | SSCROk of value
    | SCCRGiven of value * digit
    | SCCRNotACandidate of value

let set_cell_command_check_result_to_string (r : set_cell_command_check_result) : string =
    match r with
    | SSCROk value -> ""
    | SCCRGiven (value, digit) -> Printf.sprintf "Error: Cell %s has given %s" (Cell.print value.cell) (Digit.print digit)
    | SCCRNotACandidate value -> Printf.sprintf "Warning: Cell %s does not have candidate %s" (Cell.print value.cell) (Digit.print value.digit)

let setCellCommandCheck (given : given) (cellCandidates : cellCandidates) (value : value) : set_cell_command_check_result =
    let givenDigitOpt = SMap.get value.cell given in
    match givenDigitOpt with
    | Some givenDigit -> SCCRGiven (value, givenDigit)
    | None ->
        let digits = SMap.get value.cell cellCandidates in
        if OSet.contains value.digit digits then SSCROk value
        else SCCRNotACandidate value

[<NoComparison;NoEquality>]
type clear_candidate_command_parse_result =
    | CCCPROk of candidate
    | CCCPRParseError of parse_cell_results * parse_value_result
    | CCCPRWrongItemCount of int

let candidateClearCommandParse (s: puzzleShape) (item : string) (p : core.Puzzlemap.puzzleMap) : clear_candidate_command_parse_result = 
    let terms = Sset.split_char ' ' item in
    if List.length terms = 4 then 
        let parsedCell = parseCell (OSet.count s.alphabet) p.cells (List.item 1 terms) (List.item 2 terms) in
        let parsedDigit = parseValue s.alphabet (List.item 3 terms) in

        match (parsedCell, parsedDigit) with
        | (COk cell, VOk digit) -> CCCPROk (Candidate.make cell digit)
        | _ -> CCCPRParseError (parsedCell, parsedDigit)
    else CCCPRWrongItemCount (List.length terms)

[<NoComparison;NoEquality>]
type clear_candidate_command_check_result =
    | CCCCROk of candidate
    | CCCCRGiven of candidate * digit
    | CCCCRNotACandidate of candidate

let clear_candidate_command_check_result_to_string (r : clear_candidate_command_check_result) : string =
    match r with
    | CCCCROk candidate -> ""
    | CCCCRGiven (candidate, digit) -> Printf.sprintf "Error: Cell %s has given %s" (Cell.print candidate.cell) (Digit.print digit)
    | CCCCRNotACandidate candidate -> Printf.sprintf "Warning: Cell %s does not have candidate %s" (Cell.print candidate.cell) (Digit.print candidate.digit)

let candidateClearCommandCheck (given : given) (cellCandidates : cellCandidates) (candidate : candidate) : clear_candidate_command_check_result =
    let givenDigitOpt = SMap.get candidate.cell given in
    match givenDigitOpt with
    | Some givenDigit -> CCCCRGiven (candidate, givenDigit)
    | None ->
        let digits = SMap.get candidate.cell cellCandidates in
        if OSet.contains candidate.digit digits then CCCCROk candidate
        else CCCCRNotACandidate candidate

let supportedHints : Map<string, (core.Puzzlemap.puzzleMap -> cellCandidates -> core.Hint.description list)> =
    let keys =
        [
            "fh";
            "hs";
            "hp";
            "ht";
            "hq";
            "ns";
            "np";
            "nt";
            "nq";
            "pp";
            "bl";
            "x";
            "y";
        ]
        in

    let command key =
        match key with
        | "fh" -> hints.FullHouse.find
        | "hs" -> hints.Hidden.find 1
        | "hp" -> hints.Hidden.find 2
        | "ht" -> hints.Hidden.find 3
        | "hq" -> hints.Hidden.find 4
        | "ns" -> hints.Naked.find 1
        | "np" -> hints.Naked.find 2
        | "nt" -> hints.Naked.find 3
        | "nq" -> hints.Naked.find 4
        | "pp" -> hints.Intersection.pointingPairs
        | "bl" -> hints.Intersection.boxLineReductions
        | "x" -> hints.Wing.xWings
        | "y" -> hints.Wing.yWings
        in

    keys |> List.map (fun c -> (c, command c)) |> Map.ofList
