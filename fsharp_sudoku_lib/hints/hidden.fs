module Sudoku.Lib.hints.Hidden

open Sudoku.Lib.compat.oset
open Sudoku.Lib.compat.smap
open Sudoku.Lib.core.Sudoku
open Sudoku.Lib.core.Puzzlemap
open Sudoku.Lib.core.Hint

let findHidden (count : int) (p : puzzleMap) (cellCandidates : cellCandidates) (candidateSubset : digits) (primaryHouse : house) : description option = 

    let pointers = 
        p.houseCells
        |> SMap.get primaryHouse
        |> OSet.mapl (fun cell -> CandidateReduction.make cell (SMap.get cell cellCandidates))
        |> List.map (fun cr -> CandidateReduction.make cr.cell (OSet.intersect cr.candidates candidateSubset))
        |> List.filter (fun cr -> OSet.count cr.candidates > 0) 
        in

    let candidateReductions = 
        p.houseCells
        |> SMap.get primaryHouse
        |> OSet.mapl (fun cell -> CandidateReduction.make cell (SMap.get cell cellCandidates))
        |> List.map
            (fun cr -> 
                let pointerCandidates = OSet.intersect cr.candidates candidateSubset in
            
                let crs = 
                    if OSet.count pointerCandidates > 0 then OSet.difference cr.candidates candidateSubset
                    else OSet.empty()
                    in

                let candidateReduction = CandidateReduction.make cr.cell crs in
            
                candidateReduction)
        |> List.filter (fun cr -> OSet.count cr.candidates > 0) 
        in

    if List.length pointers = count && List.length candidateReductions > 0 then 

        let setCellValue = 
            if count = 1 then 
                let h = List.head pointers in
                let cell = h.cell in
                let candidate = OSet.head candidateSubset in

                let setCellValue = Value.make cell candidate in

                Some setCellValue
            else None
            in

        Some { primaryHouses = OSet.singleton primaryHouse;
               secondaryHouses = OSet.empty();
               candidateReductions = candidateReductions;
               setCellValueAction = setCellValue;
               pointers = pointers;
               focus = OSet.empty() }
    else None

let hiddenNPerHouse (count : int) (p : puzzleMap) (cellCandidates : cellCandidates) (house : house) : descriptions = 

    let houseCandidates : digits =
        p.houseCells
        |> SMap.get house
        |> OSet.mapl (fun cell -> SMap.get cell cellCandidates)
        |> OSet.concat
        in

    OSet.subsets count houseCandidates
    |> List.choose
        (fun candidateSubset -> 
            findHidden count p cellCandidates candidateSubset house)

let find (i : int) (p : puzzleMap) (cellCandidates : cellCandidates) : descriptions =
    p.houses
    |> OSet.mapl (hiddenNPerHouse i p cellCandidates)
    |> List.concat
