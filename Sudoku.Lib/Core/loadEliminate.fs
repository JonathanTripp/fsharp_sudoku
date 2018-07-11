module Sudoku.Lib.core.LoadEliminate

open Sudoku.Lib.compat.oset
open Sudoku.Lib.compat.smap

open Sudoku.Lib.core.Sudoku

let find  (p : Puzzlemap.puzzleMap) (current : current) : candidateReductions = 

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
    |> OSet.choosel
        (fun cell ->
            match reductions cell with
            | Some digits -> Some (CandidateReduction.make cell digits)
            | None -> None)

let apply (p : Puzzlemap.puzzleMap) (candidateReductions : candidateReductions) (current : current) : current = 

    let candidateReductionsLookup =
        candidateReductions
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

let description (p : Puzzlemap.puzzleMap) (candidateReductions : candidateReductions) : Hint.description =
    { primaryHouses = OSet.empty();
      secondaryHouses = OSet.empty();
      candidateReductions = candidateReductions;
      setCellValueAction = None;
      pointers = [];
      focus = OSet.empty() }

let step (p : Puzzlemap.puzzleMap) (solution : solution) (candidateReductions : candidateReductions) : solution =
    { solution with current = apply p candidateReductions solution.current;
                    steps = LoadEliminateAction :: solution.steps }

let findAndApply (p : Puzzlemap.puzzleMap) (solution : solution) : solution =
    let candidateReductions = find p solution.current in
    step p solution candidateReductions
