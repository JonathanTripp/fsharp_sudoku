module hints.Naked

open core.Sudoku
open oset
open smap

let nakedSingleCell (p : core.Puzzlemap.puzzleMap) (cellCandidates : cellCandidates) (cell : cell) : core.Hint.description option =
    let candidates = SMap.get cell cellCandidates in

    if OSet.count candidates = 1 then 
        let candidate = OSet.head candidates in

        let setCellValue = Value.make cell candidate in

        Some { primaryHouses = OSet.empty;
                secondaryHouses = OSet.empty;
                candidateReductions = OSet.empty;
                setCellValueAction = Some setCellValue;
                pointers = OSet.empty;
                focus = OSet.empty }
    else None

let nakedSingle (p : core.Puzzlemap.puzzleMap) (cellCandidates : cellCandidates) : OSet<core.Hint.description> =
    p.cells
    |> OSet.map (nakedSingleCell p cellCandidates)
    |> OSet.choose Sset.id

let findNaked (count : int) (p : core.Puzzlemap.puzzleMap) (cellCandidates : cellCandidates) (primaryHouse : house) (cellSubset : cells) : core.Hint.description option = 

    let subsetDigits =
        cellSubset
        |> OSet.map (fun cell -> SMap.get cell cellCandidates)
        |> OSet.concat
        in

    if OSet.count subsetDigits <= count then
        let candidateReductions =
            p.houseCells
            |> SMap.get primaryHouse
            |> OSet.filter (fun cell -> OSet.contains cell cellSubset = false) 
            |> OSet.map (fun cell -> 
                let candidates = SMap.get cell cellCandidates in
                CandidateReduction.make cell (OSet.intersect subsetDigits candidates))
            |> OSet.filter (fun cr -> OSet.count cr.candidates > 0)
            in

        let pointers =
            cellSubset
            |> OSet.map (fun cell -> CandidateReduction.make cell (SMap.get cell cellCandidates))
            in

        if OSet.count candidateReductions > 0 then 
            Some { primaryHouses = OSet.singleton primaryHouse;
                   secondaryHouses = OSet.empty;
                   candidateReductions = candidateReductions;
                   setCellValueAction = None;
                   pointers = pointers;
                   focus = OSet.empty }

        else None
    else None

let nakedNPerHouse (count : int) (p : core.Puzzlemap.puzzleMap) (cellCandidates : cellCandidates) (primaryHouse : house) : OSet<core.Hint.description> =
    
    let hht = 
        p.houseCells
        |> SMap.get primaryHouse
        |> OSet.filter (fun cell -> 
            let candidates = SMap.get cell cellCandidates in
            OSet.count candidates > 1 && OSet.count candidates <= count) 
        in

    Sset.setSubsets (OSet.toList hht) count
    |> OSet.ofList
    |> OSet.map (fun ss -> findNaked count p cellCandidates primaryHouse (OSet.ofList ss))
    |> OSet.choose Sset.id

let nakedN (i : int) (p : core.Puzzlemap.puzzleMap) (cellCandidates : cellCandidates) : OSet<core.Hint.description> =
    p.houses
    |> OSet.map (nakedNPerHouse i p cellCandidates )
    |> OSet.concat

let find (i : int) (p : core.Puzzlemap.puzzleMap) (cellCandidates : cellCandidates) : OSet<core.Hint.description> =
    if i = 1 then nakedSingle p cellCandidates
    else nakedN i p cellCandidates
