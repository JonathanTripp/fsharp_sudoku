module hints.Naked

open core.Sudoku
open compat.oset
open compat.smap

let nakedSingleCell (p : core.Puzzlemap.puzzleMap) (cellCandidates : cellCandidates) (cell : cell) : core.Hint.description option =
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

let nakedSingle (p : core.Puzzlemap.puzzleMap) (cellCandidates : cellCandidates) : core.Hint.description list =
    p.cells
    |> OSet.toList
    |> List.choose (nakedSingleCell p cellCandidates)

let findNaked (count : int) (p : core.Puzzlemap.puzzleMap) (cellCandidates : cellCandidates) (primaryHouse : house) (cellSubset : cells) : core.Hint.description option = 

    let subsetDigits =
        cellSubset
        |> OSet.toList
        |> List.map (fun cell -> SMap.get cell cellCandidates)
        |> OSet.concat
        in

    if OSet.count subsetDigits <= count then
        let candidateReductions =
            p.houseCells
            |> SMap.get primaryHouse
            |> OSet.toList
            |> List.filter (fun cell -> OSet.contains cell cellSubset = false) 
            |> List.map (fun cell -> 
                let candidates = SMap.get cell cellCandidates in
                CandidateReduction.make cell (OSet.intersect subsetDigits candidates))
            |> List.filter (fun cr -> OSet.count cr.candidates > 0)
            in

        let pointers =
            cellSubset
            |> OSet.toList
            |> List.map (fun cell -> CandidateReduction.make cell (SMap.get cell cellCandidates))
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

let nakedNPerHouse (count : int) (p : core.Puzzlemap.puzzleMap) (cellCandidates : cellCandidates) (primaryHouse : house) : core.Hint.description list =
    
    let hht = 
        p.houseCells
        |> SMap.get primaryHouse
        |> OSet.filter (fun cell -> 
            let candidates = SMap.get cell cellCandidates in
            OSet.count candidates > 1 && OSet.count candidates <= count) 
        in

    OSet.subsets count hht
    |> List.choose (fun ss -> findNaked count p cellCandidates primaryHouse ss)

let nakedN (i : int) (p : core.Puzzlemap.puzzleMap) (cellCandidates : cellCandidates) : core.Hint.description list =
    p.houses
    |> OSet.toList
    |> List.map (nakedNPerHouse i p cellCandidates )
    |> List.concat

let find (i : int) (p : core.Puzzlemap.puzzleMap) (cellCandidates : cellCandidates) : core.Hint.description list =
    if i = 1 then nakedSingle p cellCandidates
    else nakedN i p cellCandidates
