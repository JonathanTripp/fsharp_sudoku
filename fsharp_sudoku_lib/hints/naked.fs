module Sudoku.Lib.hints.Naked

open Sudoku.Lib.compat.oset
open Sudoku.Lib.compat.smap
open Sudoku.Lib.core.Sudoku
open Sudoku.Lib.core.Puzzlemap
open Sudoku.Lib.core.Hint

let nakedSingleCell (p : puzzleMap) (cellCandidates : cellCandidates) (cell : cell) : description option =
    let candidates = SMap.get cell cellCandidates in

    if OSet.count candidates = 1 then 
        let candidate = OSet.head candidates in

        let setCellValue = Value.make cell candidate in

        Some { primaryHouses = OSet.empty();
                secondaryHouses = OSet.empty();
                candidateReductions = [];
                setCellValueAction = Some setCellValue;
                pointers = [];
                focus = OSet.empty() }
    else None

let nakedSingle (p : puzzleMap) (cellCandidates : cellCandidates) : descriptions =
    p.cells
    |> OSet.choosel (nakedSingleCell p cellCandidates)

let findNaked (count : int) (p : puzzleMap) (cellCandidates : cellCandidates) (primaryHouse : house) (cellSubset : cells) : description option = 

    let subsetDigits : digits =
        cellSubset
        |> OSet.mapl (fun cell -> SMap.get cell cellCandidates)
        |> OSet.concat
        in

    if OSet.count subsetDigits <= count then
        let candidateReductions : candidateReductions =
            p.houseCells
            |> SMap.get primaryHouse
            |> OSet.toList
            |> List.filter (fun cell -> OSet.contains cell cellSubset = false) 
            |> List.map (fun cell -> 
                let candidates = SMap.get cell cellCandidates in
                CandidateReduction.make cell (OSet.intersect subsetDigits candidates))
            |> List.filter (fun cr -> OSet.count cr.candidates > 0)
            in

        let pointers : candidateReductions =
            cellSubset
            |> OSet.mapl (fun cell -> CandidateReduction.make cell (SMap.get cell cellCandidates))
            in

        if List.length candidateReductions > 0 then 
            Some { primaryHouses = OSet.singleton primaryHouse;
                   secondaryHouses = OSet.empty();
                   candidateReductions = candidateReductions;
                   setCellValueAction = None;
                   pointers = pointers;
                   focus = OSet.empty() }

        else None
    else None

let nakedNPerHouse (count : int) (p : puzzleMap) (cellCandidates : cellCandidates) (primaryHouse : house) : descriptions =
    
    let hht = 
        p.houseCells
        |> SMap.get primaryHouse
        |> OSet.filter (fun cell -> 
            let candidates = SMap.get cell cellCandidates in
            OSet.count candidates > 1 && OSet.count candidates <= count) 
        in

    OSet.subsets count hht
    |> List.choose (fun ss -> findNaked count p cellCandidates primaryHouse ss)

let nakedN (i : int) (p : puzzleMap) (cellCandidates : cellCandidates) : descriptions =
    p.houses
    |> OSet.mapl (nakedNPerHouse i p cellCandidates )
    |> List.concat

let find (i : int) (p : puzzleMap) (cellCandidates : cellCandidates) : descriptions =
    if i = 1 then nakedSingle p cellCandidates
    else nakedN i p cellCandidates
