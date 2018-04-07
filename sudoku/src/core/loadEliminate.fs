module core.LoadEliminate

open Sudoku
open oset
open smap

let find  (p : Puzzlemap.puzzleMap) (current : current) : OSet<candidateReduction> = 

    let reductions (cell : cell) : digits option =
        let cellContents = SMap.get cell current in
        match cellContents with
        | BigNumber _ -> None
        | PencilMarks candidates -> 
            let digits =
                p.cellHouseCells
                |> SMap.get cell
                |> OSet.choose
                    (fun cell ->
                        let houseCellContents = SMap.get cell current in
                        match houseCellContents with
                        | BigNumber digit -> Some digit
                        | PencilMarks _ -> None)
                in

            if OSet.count digits > 0 then Some digits
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
        |> OSet.toList
        |> List.map (fun cr -> (cr.cell, cr.candidates))
        |> SMap.ofList
        in

    let update (cell : cell) : cellContents =
        let cellContents = SMap.get cell current in
        match cellContents with
        | BigNumber _ -> cellContents
        | PencilMarks candidates ->
            let digitsOpt = SMap.tryGet cell candidateReductionsLookup in
            match digitsOpt with
            | Some digits ->
                OSet.difference candidates digits
                |> CellContents.make_pencil_marks
            | None -> cellContents
        in

    SMap.ofLookup update p.cells

let description (p : Puzzlemap.puzzleMap) (candidateReductions : OSet<candidateReduction>) : Hint.description =
    { primaryHouses = OSet.empty();
      secondaryHouses = OSet.empty();
      candidateReductions = candidateReductions;
      setCellValueAction = None;
      pointers = OSet.empty();
      focus = OSet.empty() }

let step (p : Puzzlemap.puzzleMap) (solution : solution) (candidateReductions : OSet<candidateReduction>) : solution =
    { solution with current = apply p candidateReductions solution.current;
                    steps = LoadEliminate :: solution.steps }

let findAndApply (p : Puzzlemap.puzzleMap) (solution : solution) : solution =
    let candidateReductions = find p solution.current in
    step p solution candidateReductions
