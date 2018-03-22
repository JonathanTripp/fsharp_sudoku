module hints.Naked

open core.Sudoku
open oset

let nakedSingleCell (p : core.Puzzlemap.puzzleMap) (cellCandidates : cellCandidates) (cell : cell) : core.Hint.description option =
    let candidates = CellCandidates.get cell cellCandidates in

    if Digits.count candidates = 1 then 
        let candidate = Digits.first candidates in

        let setCellValue = Value.make cell candidate in

        Some { primaryHouses = Houses.empty;
                secondaryHouses = Houses.empty;
                candidateReductions = OSet.empty;
                setCellValueAction = Some setCellValue;
                pointers = OSet.empty;
                focus = Digits.empty }
    else None

let nakedSingle (p : core.Puzzlemap.puzzleMap) (cellCandidates : cellCandidates) : core.Hint.description list =
    p.cells
    |> OSet.map (nakedSingleCell p cellCandidates)
    |> OSet.choose Sset.id
    |> OSet.toList

let findNaked (count : int) (p : core.Puzzlemap.puzzleMap) (cellCandidates : cellCandidates) (primaryHouse : house) (cellSubset : cells) : core.Hint.description option = 

    let subsetDigits =
        cellSubset
        |> OSet.map (fun cell -> CellCandidates.get cell cellCandidates)
        |> OSet.toList
        |> Digits.union_many
        in

    if Digits.count subsetDigits <= count then
        let candidateReductions =
            p.houseCells
            |> Smap.get House.comparer primaryHouse
            |> OSet.filter (fun cell -> OSet.contains cell cellSubset = false) 
            |> OSet.map (fun cell -> 
                let candidates = CellCandidates.get cell cellCandidates in
                CandidateReduction.make cell (Digits.intersect subsetDigits candidates))
            |> OSet.filter (fun cr -> Digits.count cr.candidates > 0)
            in

        let pointers =
            cellSubset
            |> OSet.map (fun cell -> CandidateReduction.make cell (CellCandidates.get cell cellCandidates))
            in

        if OSet.count candidateReductions > 0 then 
            Some { primaryHouses = Houses.singleton primaryHouse;
                   secondaryHouses = Houses.empty;
                   candidateReductions = candidateReductions;
                   setCellValueAction = None;
                   pointers = pointers;
                   focus = Digits.empty }

        else None
    else None

let nakedNPerHouse (count : int) (p : core.Puzzlemap.puzzleMap) (cellCandidates : cellCandidates) (primaryHouse : house) : core.Hint.description list =
    
    let hht = 
        p.houseCells
        |> Smap.get House.comparer primaryHouse
        |> OSet.filter (fun cell -> 
            let candidates = CellCandidates.get cell cellCandidates in
            Digits.count candidates > 1 && Digits.count candidates <= count) 
        in

    Sset.setSubsets (OSet.toList hht) count
    |> List.map (fun ss -> findNaked count p cellCandidates primaryHouse (OSet.ofList ss))
    |> Sset.choose Sset.id

let nakedN (i : int) (p : core.Puzzlemap.puzzleMap) (cellCandidates : cellCandidates) : core.Hint.description list =
    p.houses
    |> Houses.map (nakedNPerHouse i p cellCandidates )
    |> List.concat

let find (i : int) (p : core.Puzzlemap.puzzleMap) (cellCandidates : cellCandidates) : core.Hint.description list =
    if i = 1 then nakedSingle p cellCandidates
    else nakedN i p cellCandidates
