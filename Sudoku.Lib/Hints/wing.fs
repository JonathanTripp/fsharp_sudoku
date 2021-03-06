module Sudoku.Lib.hints.Wing

open Sudoku.Lib.compat
open Sudoku.Lib.compat.oset
open Sudoku.Lib.compat.smap

open Sudoku.Lib.core.Sudoku
open Sudoku.Lib.core.Puzzlemap
open Sudoku.Lib.core.Hint

let makeHints (p : puzzleMap) (cellCandidates : cellCandidates) (pointerCells : cells) (primaryHouses : houses) (secondaryHouses : houses) (candidate : digit) : description option = 
    let pointers : candidateReductions =
        pointerCells
        |> OSet.mapl (fun cell -> CandidateReduction.make cell (OSet.singleton candidate))
        in

    let colCells : cells =
        secondaryHouses
        |> OSet.mapl (fun house -> SMap.get house p.houseCells)
        |> OSet.concat
        in

    let candidatesReductions : candidateReductions = 
        OSet.difference colCells pointerCells
        |> OSet.mapl (fun cell -> CandidateReduction.make cell (SMap.get cell cellCandidates))
        |> List.filter (fun cr -> OSet.contains candidate cr.candidates)
        |> List.map (fun cr -> CandidateReduction.make cr.cell (OSet.singleton candidate))
        in

    if List.length candidatesReductions > 0 then
        let hint : description =
            { primaryHouses = primaryHouses;
              secondaryHouses = secondaryHouses;
              candidateReductions = candidatesReductions;
              setCellValueAction = None;
              pointers = pointers;
              focus = OSet.empty() }
            in
        Some hint
    else None

let xWingsPerHouseCandidate (p : puzzleMap) (cellCandidates : cellCandidates) (house1 : house) (house2 : house) (candidate : digit) = 

    let houseCandidateCells1 : candidateReductions =
        p.houseCells
        |> SMap.get house1
        |> OSet.mapl (fun cell -> CandidateReduction.make cell (SMap.get cell cellCandidates))
        in

    let houseCandidateCells2 : candidateReductions =
        p.houseCells
        |> SMap.get house2
        |> OSet.mapl (fun cell -> CandidateReduction.make cell (SMap.get cell cellCandidates))
        in

    let hht1 : candidateReductions =
        houseCandidateCells1
        |> List.filter (fun cr -> OSet.contains candidate cr.candidates)
        in

    let hht2 : candidateReductions =
        houseCandidateCells2
        |> List.filter (fun cr -> OSet.contains candidate cr.candidates)
        in

    match house1, house2 with
    | HRow row1, HRow row2 -> 
        let cols1 : columns = hht1 |> List.map (fun cr -> cr.cell.col) |> OSet.ofList in
        let cols2 : columns = hht2 |> List.map (fun cr -> cr.cell.col) |> OSet.ofList in

        let cols = OSet.union cols1 cols2 in

        if OSet.count cols1 = 2 && OSet.count cols2 = 2 && OSet.count cols = 2 then 
            let row1Cells =
                cols
                |> OSet.map (fun col -> Cell.make col row1)
                in

            let row2Cells = 
                cols
                |> OSet.map (fun col -> Cell.make col row2)
                in

            let pointerCells =
                [ row1Cells; row2Cells ]
                |> OSet.unionMany
                in

            let primaryHouses = OSet.ofList [ house1; house2 ] in

            let secondaryHouses =
                cols
                |> OSet.map House.make_column
                in

            makeHints p cellCandidates pointerCells primaryHouses secondaryHouses candidate

        else None

    | HColumn col1, HColumn col2 -> 
        let rows1 : rows = hht1 |> List.map (fun cr -> cr.cell.row) |> OSet.ofList in
        let rows2 : rows = hht2 |> List.map (fun cr -> cr.cell.row) |> OSet.ofList in

        let rows = OSet.union rows1 rows2 in

        if OSet.count rows1 = 2 && OSet.count rows2 = 2 && OSet.count rows = 2 then 
            let col1Cells =
                rows
                |> OSet.map (fun row -> Cell.make col1 row)
                in

            let col2Cells =
                rows
                |> OSet.map (fun row -> Cell.make col2 row)
                in

            let pointerCells =
                [ col1Cells; col2Cells ]
                |> OSet.unionMany
                in

            let primaryHouses = OSet.ofList [ house1; house2 ] in
            let secondaryHouses =
                rows
                |> OSet.map House.make_row
                in

            makeHints p cellCandidates pointerCells primaryHouses secondaryHouses candidate

        else None
    | _ -> None

let xWingsPerHouse (p : puzzleMap) (cellCandidates : cellCandidates) (house1 : house) 
    (house2 : house) : descriptions = 

    let houseCandidates1 : digits =
        p.houseCells
        |> SMap.get house1
        |> OSet.mapl (fun cell -> SMap.get cell cellCandidates)
        |> OSet.concat
        in

    let houseCandidates2 : digits =
        p.houseCells
        |> SMap.get house2
        |> OSet.mapl (fun cell -> SMap.get cell cellCandidates)
        |> OSet.concat
        in

    OSet.intersect houseCandidates1 houseCandidates2
    |> OSet.choosel (xWingsPerHouseCandidate p cellCandidates house1 house2)

let xWings (p : puzzleMap) (cellCandidates : cellCandidates) : descriptions =
    let rows = OSet.mapl House.make_row p.rows in
    let cols = OSet.mapl House.make_column p.columns in

    let rowHints1 : descriptions list list = 
        rows
        |> List.mapi 
            (fun i row1 -> 
                List.skip (i + 1) rows
                |> List.mapi
                    (fun j row2 -> xWingsPerHouse p cellCandidates row1 row2)) 
        in

    let rowHints : descriptions = 
        rowHints1
        |> List.concat
        |> List.concat
        in

    let colHints1 : descriptions list list = 
        cols
        |> List.mapi
            (fun i col1 -> 
                List.skip (i + 1) cols
                |> List.mapi
                    (fun j col2 -> xWingsPerHouse p cellCandidates col1 col2)) 
        in

    let colHints : descriptions = 
        colHints1
        |> List.concat
        |> List.concat
        in

    [ rowHints; colHints ]
    |> List.concat

let yWingsPerHouseCandidate (p : puzzleMap) (cellCandidates : cellCandidates)
    (house1 : house) (house2 : house) (houseCandidateCells1 : candidateReductions) (houseCandidateCells2 : candidateReductions) (candidate : digit) = 
    let hht1 : candidateReductions =
        houseCandidateCells1
        |> List.filter (fun cr -> OSet.contains candidate cr.candidates)
        in

    let hht2 : candidateReductions =
        houseCandidateCells2
        |> List.filter (fun cr -> OSet.contains candidate cr.candidates)
        in

    match house1, house2 with
    | HRow row1, HRow row2 -> 
        let cols1 = List.map (fun cr -> cr.cell.col) hht1 |> OSet.ofList in
        let cols2 = List.map (fun cr -> cr.cell.col) hht2 |> OSet.ofList in

        let cols : columns = OSet.union cols1 cols2 in

        if OSet.count cols1 = 2 && OSet.count cols2 = 2 && OSet.count cols = 2 then 
            let row1Cells =
                cols
                |> OSet.map (fun col -> Cell.make col row1)
                in

            let row2Cells =
                cols
                |> OSet.map (fun col -> Cell.make col row2)
                in

            let pointerCells = OSet.union row1Cells row2Cells in

            let primaryHouses = OSet.ofList [ house1; house2 ] in
            let secondaryHouses =
                cols
                |> OSet.map House.make_column
                in

            makeHints p cellCandidates pointerCells primaryHouses secondaryHouses candidate

        else None

    | HColumn col1, HColumn col2 -> 
        let rows1 = List.map (fun cr -> cr.cell.row) hht1 |> OSet.ofList in
        let rows2 = List.map (fun cr -> cr.cell.row) hht2 |> OSet.ofList in

        let rows : rows = OSet.union rows1 rows2 in

        if OSet.count rows1 = 2 && OSet.count rows2 = 2 && OSet.count rows = 2 then 
            let col1Cells = 
                rows
                |> OSet.map (fun row -> Cell.make col1 row)
                in

            let col2Cells =
                rows
                |> OSet.map (fun row -> Cell.make col2 row)
                in

            let pointerCells = OSet.union col1Cells col2Cells in

            let primaryHouses = OSet.ofList [ house1; house2 ] in
            let secondaryHouses =
                rows
                |> OSet.map House.make_row
                in

            makeHints p cellCandidates pointerCells primaryHouses secondaryHouses candidate

        else None
    | _ -> None

let yWingsPerHouse (p : puzzleMap) (cellCandidates : cellCandidates) (row1 : row) 
    (row2 : row) (col1 : column) (col2 : column)  : descriptions = 

    let cell11 = Cell.make col1 row1 in
    let cell12 = Cell.make col2 row1 in
    let cell21 = Cell.make col1 row2 in
    let cell22 = Cell.make col2 row2 in
    
    let cells = [ cell11; cell12; cell21; cell22 ] in

    let candidateCells =
        cells
        |> List.map (fun cell -> CandidateReduction.make cell (SMap.get cell cellCandidates))
        in

    let ccell11 = CandidateReduction.make cell11 (SMap.get cell11 cellCandidates) in
    let ccell12 = CandidateReduction.make cell12 (SMap.get cell12 cellCandidates) in
    let ccell21 = CandidateReduction.make cell21 (SMap.get cell21 cellCandidates) in
    let ccell22 = CandidateReduction.make cell22 (SMap.get cell22 cellCandidates) in

    let allNonEmpty =
        candidateCells
        |> List.forall (fun cr -> OSet.count cr.candidates > 0)
        in

    if allNonEmpty then 
        let triples = 
            [ (cell12, [ ccell11; ccell12; ccell22 ], cell21);
              (cell22, [ ccell12; ccell22; ccell21 ], cell11);
              (cell21, [ ccell22; ccell21; ccell11 ], cell12);
              (cell11, [ ccell21; ccell11; ccell12 ], cell22) ]
            in

        triples
        |> List.map
          (fun (pivot1, pointers, other) -> 
            let ccs = List.map (fun cr -> cr.candidates) pointers in

            let allPairs =
                ccs
                |> List.forall (fun c -> OSet.count c = 2)
                in

            if allPairs then 
                let allCandidates =
                    ccs
                    |> OSet.unionMany
                    in

                if OSet.count allCandidates = 3 then 
                    match pointers with
                    | [ left; pivot; right; _ ] -> 
                        let removee = OSet.difference allCandidates pivot.candidates in

                        if OSet.count removee = 1 && (OSet.equals left.candidates right.candidates = false) && 
                            OSet.isSubset removee (SMap.get other cellCandidates) then

                            let candidateReductions = CandidateReduction.make other removee in

                            let primaryHouses = 
                                [ HRow row1;
                                  HRow row2;
                                  HColumn col1;
                                  HColumn col2; ]
                                |> OSet.ofList
                                in

                            let desc : description =
                                { primaryHouses = primaryHouses;
                                  secondaryHouses = OSet.empty();
                                  candidateReductions = [candidateReductions];
                                  setCellValueAction = None;
                                  pointers = pointers;
                                  focus = OSet.empty() } in
                            Some desc
                        else None
                    | _ -> None
                else None
            else None)
        |> List.choose Sset.id
    else []

let yWings (p : puzzleMap) (cellCandidates : cellCandidates) : descriptions =
    let rows : row list = p.rows |> OSet.toList
    let columns = p.columns |> OSet.toList

    let hints : descriptions list list list list =
        rows
        |> List.mapi 
            (fun i row1 ->
                List.skip (i + 1) rows
                |> List.mapi 
                    (fun j row2 -> 
                        columns
                        |> List.mapi 
                            (fun k col1 -> 
                                List.skip (k + 1) columns
                                |> List.mapi
                                    (fun l col2 -> yWingsPerHouse p cellCandidates row1 row2 col1 col2)))) 
        in

    hints
    |> List.concat
    |> List.concat
    |> List.concat
    |> List.concat
