module Sudoku.Lib.core.SetCell

open Sudoku.Lib.compat.Sset
open Sudoku.Lib.compat.oset
open Sudoku.Lib.compat.smap

open Sudoku.Lib.core.Sudoku

let apply (p : Puzzlemap.puzzleMap) (value : value) (current : current) : current = 

    let update (cell : cell) : cellContents =
        let cellContents = SMap.get cell current in
        match cellContents with
        | BigNumber _ -> cellContents
        | PencilMarks candidates -> 
            let cells = SMap.get value.cell p.cellHouseCells in

            if Cell.setElemCompare value.cell cell = EQ then BigNumber value.digit
            else if OSet.contains cell cells then 
                PencilMarks (OSet.remove value.digit candidates)
            else cellContents
        in

    SMap.ofLookup update p.cells

[<NoComparison;NoEquality>]
type setCellDigitError = 
    { cell : cell;
      candidate : digit;
      digit : digit }

let try' (cell : cell) (candidate : digit) (cellCandidates : cellCandidates) : value option = 
    let candidates = SMap.get cell cellCandidates in

    if OSet.contains candidate candidates then
        Some (Value.make cell candidate)
    else None

let description (p : Puzzlemap.puzzleMap) (setCellValue : value) : Hint.description =
    { primaryHouses = OSet.empty();
      secondaryHouses = OSet.empty();
      candidateReductions = [];
      setCellValueAction = Some setCellValue;
      pointers = [];
      focus = OSet.empty() }

let step (p : Puzzlemap.puzzleMap) (setCellValue : value) (solution : solution) : solution =
    { solution with current = apply p setCellValue solution.current;
                    steps = (Placement setCellValue) :: solution.steps }
