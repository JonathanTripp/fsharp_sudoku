module core.SetCell

open Sudoku
open oset

let apply (p : Puzzlemap.puzzleMap) (value : value) (current : current) : current = 

    let update (cell : cell) : cellContents =
        let cellContents = Current.get cell current in
        match cellContents with
        | BigNumber _ -> cellContents
        | PencilMarks candidates -> 
            let cells = Smap.get Cell.comparer value.cell p.cellHouseCells in

            if value.cell = cell then BigNumber value.digit
            else if OSet.contains cell cells then 
                PencilMarks (Digits.remove value.digit candidates)
            else cellContents
        in

    Cells.ofLookup update p.cells
    |> Current.make

type setCellDigitError = 
    { cell : cell;
      candidate : digit;
      digit : digit }

let try' (cell : cell) (candidate : digit) (cellCandidates : cellCandidates) : value option = 
    let candidates = CellCandidates.get cell cellCandidates in

    if Digits.contains candidate candidates then
        Some (Value.make cell candidate)
    else None

let description (p : Puzzlemap.puzzleMap) (setCellValue : value) : Hint.description =
    { primaryHouses = Houses.empty;
      secondaryHouses = Houses.empty;
      candidateReductions = OSet.empty;
      setCellValueAction = Some setCellValue;
      pointers = OSet.empty;
      focus = Digits.empty }

let step (p : Puzzlemap.puzzleMap) (setCellValue : value) (solution : solution) : solution =
    { solution with current = apply p setCellValue solution.current;
                    steps = (Placement setCellValue) :: solution.steps }
