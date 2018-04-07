module hints.Intersection

open core.Sudoku
open oset
open smap

type cellHouses = SMap<cell, houses>

let intersectionsPerHouse (p : core.Puzzlemap.puzzleMap) (cellCandidates : cellCandidates) (primaryHouse : house) (secondaryHouseLookups : cellHouses) : core.Hint.description list = 

    let primaryHouseCandidates = 
        p.houseCells
        |> SMap.get primaryHouse
        |> OSet.map (fun cell -> SMap.get cell cellCandidates)
        |> OSet.concat
        in

    let uniqueSecondaryForCandidate (candidate : digit) : core.Hint.description list = 
        let pointerCells = 
            p.houseCells
            |> SMap.get primaryHouse
            |> OSet.filter (fun cell -> 
                let candidates = SMap.get cell cellCandidates in
                OSet.contains candidate candidates) 
            in

        let pointers  = 
            pointerCells
            |> OSet.map (fun cell -> CandidateReduction.make cell (OSet.singleton candidate))
            in

        let hintsPerSecondaryHouse (secondaryHouses : houses) : core.Hint.description option = 
            if OSet.count pointerCells > 1 && OSet.count secondaryHouses = 1 then 
                let primaryHouseCells =
                    p.houseCells
                    |> SMap.get primaryHouse
                    in

                let secondaryHouse = OSet.item 0 secondaryHouses in
                let secondaryHouseCells = SMap.get secondaryHouse p.houseCells in

                let otherHouseCells = OSet.difference secondaryHouseCells primaryHouseCells in
                
                let candidateReductions = 
                    otherHouseCells
                    |> OSet.filter (fun cell -> 
                        let candidates = SMap.get cell cellCandidates in
                        OSet.contains candidate candidates)
                    |> OSet.map (fun cell -> CandidateReduction.make cell (OSet.singleton candidate))
                    in

                if OSet.count candidateReductions > 0 then 
                    Some { primaryHouses = OSet.singleton primaryHouse;
                           secondaryHouses = OSet.singleton secondaryHouse;
                           candidateReductions = candidateReductions;
                           setCellValueAction = None;
                           pointers = pointers;
                           focus = OSet.empty }
                else None
            else None
            in

        pointerCells
        |> OSet.toList
        |> List.choose (fun cell -> 
                            SMap.get cell secondaryHouseLookups
                            |> hintsPerSecondaryHouse)
        in

    primaryHouseCandidates
    |> OSet.map uniqueSecondaryForCandidate
    |> OSet.toList
    |> List.concat

let pointingPairsPerBox (p : core.Puzzlemap.puzzleMap) (cellCandidates : cellCandidates) (primaryHouse : house) : core.Hint.description list =
    let cellLines (cell : cell) =
        [ HRow cell.row; HColumn cell.col ]
        |> OSet.ofList
        in

    let secondaryHouseLookups =
        SMap.ofLookup cellLines p.cells
        in

    intersectionsPerHouse p cellCandidates primaryHouse secondaryHouseLookups

let boxLineReductionsPerHouse (p : core.Puzzlemap.puzzleMap) (cellCandidates : cellCandidates) (primaryHouse : house) : core.Hint.description list = 
    let cellBox (cell : cell) =
        [ SMap.get cell p.cellBox |> House.make_box ]
        |> OSet.ofList
        in

    let secondaryHouseLookups =
        SMap.ofLookup cellBox p.cells
        in

    intersectionsPerHouse p cellCandidates primaryHouse secondaryHouseLookups

let pointingPairs (p : core.Puzzlemap.puzzleMap) (cellCandidates : cellCandidates) : core.Hint.description list =
    p.boxes
    |> OSet.toList
    |> List.map House.make_box
    |> List.map (pointingPairsPerBox p cellCandidates) 
    |> List.concat

let boxLineReductions (p : core.Puzzlemap.puzzleMap) (cellCandidates : cellCandidates) : core.Hint.description list =
    let rowHints =
        p.rows
        |> OSet.toList
        |> List.map House.make_row
        |> List.map (boxLineReductionsPerHouse p cellCandidates)
        |> List.concat
        in

    let colHints =
        p.columns
        |> OSet.toList
        |> List.map House.make_column
        |> List.map (boxLineReductionsPerHouse p cellCandidates)
        |> List.concat
        in

    [ rowHints; colHints ]
    |> List.concat
