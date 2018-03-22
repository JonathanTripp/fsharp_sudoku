module hints.Intersection

open core.Sudoku
open oset

type cellHouses = (cell * house list) list

let intersectionsPerHouse (p : core.Puzzlemap.puzzleMap) (cellCandidates : cellCandidates) (primaryHouse : house) (secondaryHouseLookups : cellHouses) : core.Hint.description list = 

    let primaryHouseCandidates = 
        p.houseCells
        |> Smap.get House.comparer primaryHouse
        |> OSet.map (fun cell -> CellCandidates.get cell cellCandidates)
        |> OSet.toList
        |> Digits.union_many
        in

    let uniqueSecondaryForCandidate (candidate : digit) : OSet<core.Hint.description> = 
        let pointerCells = 
            p.houseCells
            |> Smap.get House.comparer primaryHouse
            |> OSet.filter (fun cell -> 
                let candidates = CellCandidates.get cell cellCandidates in
                Digits.contains candidate candidates) 
            in

        let pointers  = 
            pointerCells
            |> OSet.map (fun cell -> CandidateReduction.make cell (Digits.singleton candidate))
            in

        let hintsPerSecondaryHouse (secondaryHouses : house list) : core.Hint.description option = 
            if OSet.count pointerCells > 1 && List.length secondaryHouses = 1 then 
                let primaryHouseCells =
                    p.houseCells
                    |> Smap.get House.comparer primaryHouse
                    in

                let secondaryHouse = List.nth secondaryHouses 0 in
                let secondaryHouseCells = Smap.get House.comparer secondaryHouse p.houseCells in

                let otherHouseCells = OSet.difference secondaryHouseCells primaryHouseCells in
                
                let candidateReductions = 
                    otherHouseCells
                    |> OSet.filter (fun cell -> 
                        let candidates = CellCandidates.get cell cellCandidates in
                        Digits.contains candidate candidates)
                    |> OSet.map (fun cell -> CandidateReduction.make cell (Digits.singleton candidate))
                    in

                if OSet.count candidateReductions > 0 then 
                    Some { primaryHouses = Houses.singleton primaryHouse;
                           secondaryHouses = Houses.singleton secondaryHouse;
                           candidateReductions = candidateReductions;
                           setCellValueAction = None;
                           pointers = pointers;
                           focus = Digits.empty }
                else None
            else None
            in

        pointerCells
        |> OSet.choose (fun cell -> 
                            Smap.get Cell.comparer cell secondaryHouseLookups
                            |> hintsPerSecondaryHouse)
        in

    primaryHouseCandidates
    |> Digits.map uniqueSecondaryForCandidate
    |> List.map (OSet.toList)
    |> List.concat

let pointingPairsPerBox (p : core.Puzzlemap.puzzleMap) (cellCandidates : cellCandidates) (primaryHouse : house) : core.Hint.description list =
    let cellLines (cell : cell) =
        [ HRow cell.row; HColumn cell.col ]
        in

    let secondaryHouseLookups =
        Cells.ofLookup cellLines p.cells
        in

    intersectionsPerHouse p cellCandidates primaryHouse secondaryHouseLookups

let boxLineReductionsPerHouse (p : core.Puzzlemap.puzzleMap) (cellCandidates : cellCandidates) (primaryHouse : house) : core.Hint.description list = 
    let cellBox (cell : cell) =
        [ Smap.get Cell.comparer cell p.cellBox |> House.make_box ]
        in

    let secondaryHouseLookups =
        Cells.ofLookup cellBox p.cells
        in

    intersectionsPerHouse p cellCandidates primaryHouse secondaryHouseLookups

let pointingPairs (p : core.Puzzlemap.puzzleMap) (cellCandidates : cellCandidates) : core.Hint.description list =
    p.boxes
    |> List.map House.make_box
    |> List.map (pointingPairsPerBox p cellCandidates) 
    |> List.concat

let boxLineReductions (p : core.Puzzlemap.puzzleMap) (cellCandidates : cellCandidates) : core.Hint.description list =
    let rowHints =
        p.rows
        |> OSet.map House.make_row
        |> OSet.toList
        |> Houses.make
        |> Houses.map (boxLineReductionsPerHouse p cellCandidates)
        |> List.concat
        in

    let colHints =
        p.columns
        |> OSet.map House.make_column
        |> OSet.toList
        |> Houses.make
        |> Houses.map (boxLineReductionsPerHouse p cellCandidates)
        |> List.concat
        in

    [ rowHints; colHints ]
    |> List.concat
