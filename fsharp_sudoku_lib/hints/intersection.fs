module hints.Intersection

open compat.oset
open compat.smap
open core.Sudoku
open core.Puzzlemap
open core.Hint

type cellHouses = SMap<cell, houses>

let intersectionsPerHouse (p : puzzleMap) (cellCandidates : cellCandidates) (primaryHouse : house) (secondaryHouseLookups : cellHouses) : descriptions = 

    let primaryHouseCandidates : digits = 
        p.houseCells
        |> SMap.get primaryHouse
        |> OSet.mapl (fun cell -> SMap.get cell cellCandidates)
        |> OSet.concat
        in

    let uniqueSecondaryForCandidate (candidate : digit) : descriptions = 
        let pointerCells : cells = 
            p.houseCells
            |> SMap.get primaryHouse
            |> OSet.filter (fun cell -> 
                let candidates = SMap.get cell cellCandidates in
                OSet.contains candidate candidates) 
            in

        let pointers : candidateReductions = 
            pointerCells
            |> OSet.toList
            |> List.map (fun cell -> CandidateReduction.make cell (OSet.singleton candidate))
            in

        let hintsPerSecondaryHouse (secondaryHouses : houses) : description option = 
            if OSet.count pointerCells > 1 && OSet.count secondaryHouses = 1 then 
                let primaryHouseCells : cells =
                    p.houseCells
                    |> SMap.get primaryHouse
                    in

                let secondaryHouse : house = OSet.item 0 secondaryHouses in
                let secondaryHouseCells : cells = SMap.get secondaryHouse p.houseCells in

                let otherHouseCells : cells = OSet.difference secondaryHouseCells primaryHouseCells in
                
                let candidateReductions : candidateReductions = 
                    otherHouseCells
                    |> OSet.toList
                    |> List.filter (fun cell -> 
                        let candidates = SMap.get cell cellCandidates in
                        OSet.contains candidate candidates)
                    |> List.map (fun cell -> CandidateReduction.make cell (OSet.singleton candidate))
                    in

                if List.length candidateReductions > 0 then 
                    Some { primaryHouses = OSet.singleton primaryHouse;
                           secondaryHouses = OSet.singleton secondaryHouse;
                           candidateReductions = candidateReductions;
                           setCellValueAction = None;
                           pointers = pointers;
                           focus = OSet.empty() }
                else None
            else None
            in

        pointerCells
        |> OSet.choosel (fun cell -> 
                            SMap.get cell secondaryHouseLookups
                            |> hintsPerSecondaryHouse)
        in

    primaryHouseCandidates
    |> OSet.mapl uniqueSecondaryForCandidate
    |> List.concat

let pointingPairsPerBox (p : puzzleMap) (cellCandidates : cellCandidates) (primaryHouse : house) : descriptions =
    let cellLines (cell : cell) : houses =
        [ HRow cell.row; HColumn cell.col ]
        |> OSet.ofList
        in

    let secondaryHouseLookups : SMap<cell, houses> =
        SMap.ofLookup cellLines p.cells
        in

    intersectionsPerHouse p cellCandidates primaryHouse secondaryHouseLookups

let boxLineReductionsPerHouse (p : puzzleMap) (cellCandidates : cellCandidates) (primaryHouse : house) : descriptions = 
    let cellBox (cell : cell) =
        [ SMap.get cell p.cellBox |> House.make_box ]
        |> OSet.ofList
        in

    let secondaryHouseLookups =
        SMap.ofLookup cellBox p.cells
        in

    intersectionsPerHouse p cellCandidates primaryHouse secondaryHouseLookups

let pointingPairs (p : puzzleMap) (cellCandidates : cellCandidates) : descriptions =
    p.boxes
    |> OSet.map House.make_box
    |> OSet.mapl (pointingPairsPerBox p cellCandidates) 
    |> List.concat

let boxLineReductions (p : puzzleMap) (cellCandidates : cellCandidates) : descriptions =
    let rowHints =
        p.rows
        |> OSet.map House.make_row
        |> OSet.mapl (boxLineReductionsPerHouse p cellCandidates)
        |> List.concat
        in

    let colHints =
        p.columns
        |> OSet.map House.make_column
        |> OSet.mapl (boxLineReductionsPerHouse p cellCandidates)
        |> List.concat
        in

    [ rowHints; colHints ]
    |> List.concat
