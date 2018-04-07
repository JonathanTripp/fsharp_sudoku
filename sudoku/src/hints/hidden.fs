module hints.Hidden

open core.Sudoku
open oset
open smap

let findHidden (count : int) (p : core.Puzzlemap.puzzleMap) (cellCandidates : cellCandidates) (candidateSubset : digits) (primaryHouse : house) : core.Hint.description option = 

    let pointers = 
        p.houseCells
        |> SMap.get primaryHouse 
        |> OSet.map (fun cell -> CandidateReduction.make cell (SMap.get cell cellCandidates))
        |> OSet.map (fun cr -> CandidateReduction.make cr.cell (OSet.intersect cr.candidates candidateSubset))
        |> OSet.filter (fun cr -> OSet.count cr.candidates > 0) 
        in

    let candidateReductions = 
        p.houseCells
        |> SMap.get primaryHouse
        |> OSet.map (fun cell -> CandidateReduction.make cell (SMap.get cell cellCandidates))
        |> OSet.map
            (fun cr -> 
                let pointerCandidates = OSet.intersect cr.candidates candidateSubset in
            
                let crs = 
                    if OSet.count pointerCandidates > 0 then OSet.difference cr.candidates candidateSubset
                    else OSet.empty()
                    in

                let candidateReduction = CandidateReduction.make cr.cell crs in
            
                candidateReduction)
        |> OSet.filter (fun cr -> OSet.count cr.candidates > 0) 
        in

    if OSet.count pointers = count && OSet.count candidateReductions > 0 then 

        let setCellValue = 
            if count = 1 then 
                let h = OSet.head pointers in
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

let hiddenNPerHouse (count : int) (p : core.Puzzlemap.puzzleMap) (cellCandidates : cellCandidates) (house : house) : core.Hint.description list = 

    let houseCandidates =
        p.houseCells
        |> SMap.get house
        |> OSet.map (fun cell -> SMap.get cell cellCandidates)
        |> OSet.concat
        in

    OSet.subsets count houseCandidates
    |> List.choose
        (fun candidateSubset -> 
            findHidden count p cellCandidates candidateSubset house)

let find (i : int) (p : core.Puzzlemap.puzzleMap) (cellCandidates : cellCandidates) : core.Hint.description list =
    p.houses
    |> OSet.toList
    |> List.map (hiddenNPerHouse i p cellCandidates)
    |> List.concat
