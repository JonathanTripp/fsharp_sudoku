﻿module hints.wing

open core.puzzlemap
open core.sudoku
open hints

let makeHints candidateLookup houseCells pointerCells primaryHouses secondaryHouses candidate = 
    let pointers = 
        Set.map (fun cell -> 
            { CandidateReduction.cell = cell
              symbols = set [ candidate ] }) pointerCells
    
    let colCells = Set.map houseCells secondaryHouses |> Set.unionMany
    let secondaryCells = Set.difference colCells pointerCells

    let colCandidatesCells = Set.map (fun cell -> (candidateLookup cell, cell)) secondaryCells
    let potentialCells = Set.filter (fun (candidates, _) -> Set.contains candidate candidates) colCandidatesCells
    
    let candidatesReductions = 
        Set.map (fun (_, cell) -> 
            { CandidateReduction.cell = cell
              symbols = set [ candidate ] }) potentialCells

    if Set.count candidatesReductions > 0 then 
        Some { HintDescription.candidateReductions = candidatesReductions
               primaryHouses = primaryHouses
               secondaryHouses = secondaryHouses
               pointers = pointers
               setCellValue = None }
    else None

let xWingsPerHouseCandidate (candidateLookup : Cell -> Set<Candidate>) (houseCells : House -> Set<Cell>) 
    (house1 : House) (house2 : House) houseCandidateCells1 houseCandidateCells2 (candidate : Candidate) = 
    let hht1 = Set.filter (fun (candidates, _) -> Set.contains candidate candidates) houseCandidateCells1
    let hht2 = Set.filter (fun (candidates, _) -> Set.contains candidate candidates) houseCandidateCells2

    match house1, house2 with
    | Row row1, Row row2 -> 
        let cols1 = Set.map (fun (_, cell) -> cell.col) hht1
        let cols2 = Set.map (fun (_, cell) -> cell.col) hht2

        let cols = Set.union cols1 cols2

        if Set.count cols1 = 2 && Set.count cols2 = 2 && Set.count cols = 2 then 
            let row1Cells = 
                Set.map (fun col -> 
                    { Cell.col = col
                      row = row1 }) cols
            
            let row2Cells = 
                Set.map (fun col -> 
                    { Cell.col = col
                      row = row2 }) cols
            
            let pointerCells = Set.union row1Cells row2Cells

            let primaryHouses = set [ house1; house2 ]
            let secondaryHouses = Set.map Column cols

            makeHints candidateLookup houseCells pointerCells primaryHouses secondaryHouses candidate

        else None

    | Column col1, Column col2 -> 
        let rows1 = Set.map (fun (_, cell) -> cell.row) hht1
        let rows2 = Set.map (fun (_, cell) -> cell.row) hht2

        let rows = Set.union rows1 rows2

        if Set.count rows1 = 2 && Set.count rows2 = 2 && Set.count rows = 2 then 
            let col1Cells = 
                Set.map (fun row -> 
                    { Cell.col = col1
                      row = row }) rows
            
            let col2Cells = 
                Set.map (fun row -> 
                    { Cell.col = col2
                      row = row }) rows
            
            let pointerCells = Set.union col1Cells col2Cells

            let primaryHouses = set [ house1; house2 ]
            let secondaryHouses = Set.map Row rows

            makeHints candidateLookup houseCells pointerCells primaryHouses secondaryHouses candidate

        else None
    | _ -> None

let xWingsPerHouse (candidateLookup : Cell -> Set<Candidate>) (houseCells : House -> Set<Cell>) (house1 : House) 
    (house2 : House) = 
    let cells1 = houseCells house1

    let candidateCells1 = Set.map (fun cell -> ((candidateLookup cell), cell)) cells1

    let houseCandidates1 = Set.map fst candidateCells1 |> Set.unionMany

    let cells2 = houseCells house2

    let candidateCells2 = Set.map (fun cell -> ((candidateLookup cell), cell)) cells2

    let houseCandidates2 = Set.map fst candidateCells2 |> Set.unionMany

    let commonHouseCandidates = Set.intersect houseCandidates1 houseCandidates2

    Seq.map (xWingsPerHouseCandidate candidateLookup houseCells house1 house2 candidateCells1 candidateCells2) 
        commonHouseCandidates

let xWingFind (candidateLookup : Cell -> Set<Candidate>) (houseCells : House -> Set<Cell>) (rs : Row list) 
    (cs : Column list) = 

    let rows = List.map Row rs

    let cols = List.map Column cs

    let rowHints1 = 
        Seq.mapi 
            (fun i row1 -> 
            Seq.mapi (fun j row2 -> xWingsPerHouse candidateLookup houseCells row1 row2) (Seq.skip (i + 1) rows)) rows
    
    let rowHints = 
        rowHints1
        |> Seq.concat
        |> Seq.concat
        |> Seq.choose id
        |> Seq.toList
    
    let colHints1 = 
        Seq.mapi 
            (fun i col1 -> 
            Seq.mapi (fun j col2 -> xWingsPerHouse candidateLookup houseCells col1 col2) (Seq.skip (i + 1) cols)) cols
    
    let colHints = 
        colHints1
        |> Seq.concat
        |> Seq.concat
        |> Seq.choose id
        |> Seq.toList

    List.concat [ rowHints; colHints ]

let yWingsPerHouseCandidate (candidateLookup : Cell -> Set<Candidate>) (houseCells : House -> Set<Cell>) 
    (house1 : House) (house2 : House) houseCandidateCells1 houseCandidateCells2 (candidate : Candidate) = 
    let hht1 = Set.filter (fun (candidates, _) -> Set.contains candidate candidates) houseCandidateCells1
    let hht2 = Set.filter (fun (candidates, _) -> Set.contains candidate candidates) houseCandidateCells2

    match house1, house2 with
    | Row row1, Row row2 -> 
        let cols1 = Set.map (fun (_, cell) -> cell.col) hht1
        let cols2 = Set.map (fun (_, cell) -> cell.col) hht2

        let cols = Set.union cols1 cols2

        if Set.count cols1 = 2 && Set.count cols2 = 2 && Set.count cols = 2 then 
            let row1Cells = 
                Set.map (fun col -> 
                    { Cell.col = col
                      row = row1 }) cols
            
            let row2Cells = 
                Set.map (fun col -> 
                    { Cell.col = col
                      row = row2 }) cols
            
            let pointerCells = Set.union row1Cells row2Cells

            let primaryHouses = set [ house1; house2 ]
            let secondaryHouses = Set.map Column cols

            makeHints candidateLookup houseCells pointerCells primaryHouses secondaryHouses candidate

        else None

    | Column col1, Column col2 -> 
        let rows1 = Set.map (fun (_, cell) -> cell.row) hht1
        let rows2 = Set.map (fun (_, cell) -> cell.row) hht2

        let rows = Set.union rows1 rows2
        if Set.count rows1 = 2 && Set.count rows2 = 2 && Set.count rows = 2 then 
            let col1Cells = 
                Set.map (fun row -> 
                    { Cell.col = col1
                      row = row }) rows
            
            let col2Cells = 
                Set.map (fun row -> 
                    { Cell.col = col2
                      row = row }) rows
            
            let pointerCells = Set.union col1Cells col2Cells

            let primaryHouses = set [ house1; house2 ]
            let secondaryHouses = Set.map Row rows

            makeHints candidateLookup houseCells pointerCells primaryHouses secondaryHouses candidate

        else None
    | _ -> None

let yWingsPerHouse (candidateLookup : Cell -> Set<Candidate>) (houseCells : House -> Set<Cell>) (row1 : Row) 
    (row2 : Row) (col1 : Column) (col2 : Column) = 
    let cell11 = 
        { Cell.col = col1
          row = row1 }
    
    let cell12 = 
        { Cell.col = col2
          row = row1 }
    
    let cell21 = 
        { Cell.col = col1
          row = row2 }
    
    let cell22 = 
        { Cell.col = col2
          row = row2 }
    
    let cells = [ cell11; cell12; cell21; cell22 ]

    let candidateCells = List.map (fun cell -> ((candidateLookup cell), cell)) cells

    let ccell11 = ((candidateLookup cell11), cell11)
    let ccell12 = ((candidateLookup cell12), cell12)
    let ccell21 = ((candidateLookup cell21), cell21)
    let ccell22 = ((candidateLookup cell22), cell22)

    let allNonEmpty = List.forall (fun (c, _) -> Set.count c > 0) candidateCells

    if allNonEmpty then 
        let triples = 
            [ (cell12, [ ccell11; ccell12; ccell22 ], cell21)
              (cell22, [ ccell12; ccell22; ccell21 ], cell11)
              (cell21, [ ccell22; ccell21; ccell11 ], cell12)
              (cell11, [ ccell21; ccell11; ccell12 ], cell22) ]

        List.map (fun (pivot1, triple, other) -> 
            let ccs = List.map fst triple

            let allPairs = List.forall (fun c -> Set.count c = 2) ccs

            if allPairs then 
                let allCandidates = Set.unionMany ccs

                if Set.count allCandidates = 3 then 
                    match triple with
                    | left :: pivot :: right :: _ -> 
                        let removee = Set.difference allCandidates (fst pivot)

                        if Set.count removee = 1 && ((fst left) <> (fst right)) && 
                            Set.isSubset removee (candidateLookup other) then

                            let candidateReductions = { CandidateReduction.cell = other; symbols = removee }

                            let pointers = List.map (fun (cr, cell) -> { CandidateReduction.cell = cell; symbols = cr } ) triple
                                           |> Set.ofList

                            Some { HintDescription.candidateReductions = set [ candidateReductions ]
                                   primaryHouses = 
                                       set [ Row row1
                                             Row row2
                                             Column col1
                                             Column col2 ]
                                   secondaryHouses = set []
                                   pointers = pointers
                                   setCellValue = None }
                        else None
                    | _ -> None
                else None
            else None) triples
    else []

let yWingFind (candidateLookup : Cell -> Set<Candidate>) (houseCells : House -> Set<Cell>) (rows : Row list) 
    (cols : Column list) = 

    let hints = 
        Seq.mapi 
            (fun i row1 -> 
            Seq.mapi 
                (fun j row2 -> 
                Seq.mapi 
                    (fun k col1 -> 
                    Seq.mapi (fun l col2 -> yWingsPerHouse candidateLookup houseCells row1 row2 col1 col2) 
                        (Seq.skip (k + 1) cols)) cols) (Seq.skip (i + 1) rows)) rows

    hints
    |> Seq.concat
    |> Seq.concat
    |> Seq.concat
    |> Seq.concat
    |> Seq.choose id
    |> Seq.toList