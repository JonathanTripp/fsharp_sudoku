module hints.Hidden

open core.Sudoku
open oset

let findHidden (count : int) (p : core.Puzzlemap.puzzleMap) (cellCandidates : cellCandidates) (candidateSubset : digits) (primaryHouse : house) : core.Hint.description option = 

    let pointers = 
        p.houseCells
        |> Smap.get House.comparer primaryHouse 
        |> OSet.map (fun cell -> CandidateReduction.make cell (CellCandidates.get cell cellCandidates))
        |> OSet.map (fun cr -> CandidateReduction.make cr.cell (Digits.intersect cr.candidates candidateSubset))
        |> OSet.filter (fun cr -> Digits.count cr.candidates > 0) 
        in

    let candidateReductions = 
        p.houseCells
        |> Smap.get House.comparer primaryHouse
        |> OSet.map (fun cell -> CandidateReduction.make cell (CellCandidates.get cell cellCandidates))
        |> OSet.map
            (fun cr -> 
                let pointerCandidates = Digits.intersect cr.candidates candidateSubset in
            
                let crs = 
                    if Digits.count pointerCandidates > 0 then Digits.difference cr.candidates candidateSubset
                    else Digits.empty
                    in

                let candidateReduction = CandidateReduction.make cr.cell crs in
            
                candidateReduction)
        |> OSet.filter (fun cr -> Digits.count cr.candidates > 0) 
        in

    if OSet.count pointers = count && OSet.count candidateReductions > 0 then 

        let setCellValue = 
            if count = 1 then 
                let h = OSet.head pointers in
                let cell = h.cell in
                let candidate = Digits.first candidateSubset in

                let setCellValue = Value.make cell candidate in

                Some setCellValue
            else None
            in

        Some { primaryHouses = Houses.singleton primaryHouse;
               secondaryHouses = Houses.empty;
               candidateReductions = candidateReductions;
               setCellValueAction = setCellValue;
               pointers = pointers;
               focus = Digits.empty }
    else None

let hiddenNPerHouse (count : int) (p : core.Puzzlemap.puzzleMap) (cellCandidates : cellCandidates) (house : house) : core.Hint.description list = 

    let houseCandidates =
        p.houseCells
        |> Smap.get House.comparer house
        |> OSet.map (fun cell -> CellCandidates.get cell cellCandidates)
        |> OSet.toList
        |> Digits.union_many
        in

    Sset.setSubsets (Digits.to_list houseCandidates) count
    |> Sset.choose
        (fun candidateSubset -> 
            findHidden count p cellCandidates (Digits.make candidateSubset) house)

let find (i : int) (p : core.Puzzlemap.puzzleMap) (cellCandidates : cellCandidates) : core.Hint.description list =
    p.houses
    |> Houses.map (hiddenNPerHouse i p cellCandidates)
    |> List.concat
