module core.LoadEliminate

open Sudoku
open oset

let find  (p : Puzzlemap.puzzleMap) (current : current) : OSet<candidateReduction> = 

    let reductions (cell : cell) : digits option =
        let cellContents = Current.get cell current in
        match cellContents with
        | BigNumber _ -> None
        | PencilMarks candidates -> 
            let digits =
                p.cellHouseCells
                |> Smap.get Cell.comparer cell
                |> OSet.choose
                    (fun cell ->
                        let houseCellContents = Current.get cell current in
                        match houseCellContents with
                        | BigNumber digit -> Some digit
                        | PencilMarks _ -> None)
                |> OSet.toList
                |> Digits.make in

            if Digits.count digits > 0 then Some digits
            else None
        in

    p.cells
    |> OSet.choose
        (fun cell ->
            match reductions cell with
            | Some digits -> Some (CandidateReduction.make cell digits)
            | None -> None)

let apply (p : Puzzlemap.puzzleMap) (candidateReductions : OSet<candidateReduction>) (current : current) : current = 

    let candidateReductionsLookup =
        candidateReductions
        |> OSet.map (fun cr -> (cr.cell, cr.candidates))
        |> OSet.toList
        in

    let update (cell : cell) : cellContents =
        let cellContents = Current.get cell current in
        match cellContents with
        | BigNumber _ -> cellContents
        | PencilMarks candidates ->
            let digitsOpt = Smap.tryGet Cell.comparer cell candidateReductionsLookup in
            match digitsOpt with
            | Some digits ->
                Digits.difference candidates digits
                |> CellContents.make_pencil_marks
            | None -> cellContents
        in

    Cells.ofLookup update p.cells
    |> Current.make

let description (p : Puzzlemap.puzzleMap) (candidateReductions : OSet<candidateReduction>) : Hint.description =
    { primaryHouses = OSet.empty;
      secondaryHouses = OSet.empty;
      candidateReductions = candidateReductions;
      setCellValueAction = None;
      pointers = OSet.empty;
      focus = Digits.empty }

let step (p : Puzzlemap.puzzleMap) (solution : solution) (candidateReductions : OSet<candidateReduction>) : solution =
    { solution with current = apply p candidateReductions solution.current;
                    steps = LoadEliminate :: solution.steps }

let findAndApply (p : Puzzlemap.puzzleMap) (solution : solution) : solution =
    let candidateReductions = find p solution.current in
    step p solution candidateReductions
