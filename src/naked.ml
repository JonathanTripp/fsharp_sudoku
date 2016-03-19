open Sset
open Smap
open Sudoku
open Puzzlemap
open hints

let findNaked (count : int) (p : puzzleMap) (cellCandidates : cellCandidates) (primaryHouse : house) (cellSubset : cells) = 

    let subsetDigits =
        cellSubset
        |> Cells.map (SMap.get cellCandidates)
        |> Digits.unionMany
        in

    if Digits.count subsetDigits <= count then
        let candidateReductions =
            primaryHouse
            |> SMap.get p.houseCells
            |> Cells.filter (fun cell -> Cells.contains cell cellSubset = false) 
            |> Cells.map (fun cell -> 
                let candidates = SMap.get cellCandidates cell in
                makeCandidateReduction cell (Digits.intersect subsetDigits candidates))
            |> CandidateReductions.ofSet
            |> CandidateReductions.filter (fun cr -> Digits.count cr.candidates > 0)
            in

        let pointers =
            cellSubset
            |> Cells.map (fun cell -> makeCandidateReduction cell (SMap.get cellCandidates cell))
            |> CandidateReductions.ofSet
            in

        if CandidateReductions.count candidateReductions > 0 then 
            Some { hintDescription.primaryHouses = Houses.singleton primaryHouse;
                   secondaryHouses = Houses.empty;
                   candidateReductions = candidateReductions;
                   setCellValueAction = None;
                   pointers = pointers;
                   focus = Digits.empty }

        else None
    else None

let nakedNPerHouse (count : int) (p : puzzleMap) (cellCandidates : cellCandidates)  (primaryHouse : house) : hintDescription list =
    
    let hht = 
        primaryHouse
        |> SMap.get p.houseCells
        |> Cells.filter (fun cell -> 
            let candidates = SMap.get cellCandidates cell in
            Digits.count candidates > 1 && Digits.count candidates <= count) 
        in

    setSubsets (Cells.toList hht) count
    |> List.map (fun ss -> findNaked count p cellCandidates primaryHouse (Cells.ofSet ss))
    |> List.choose id

let nakedSingle (p : puzzleMap) (cellCandidates : cellCandidates) : hintDescription list =

    p.cells
    |> List.map (fun cell -> 
        let candidates = SMap.get cellCandidates cell in

        if Digits.count candidates = 1 then 
            let candidate = first candidates in

            let setCellValue = makeValue cell candidate in

            Some { hintDescription.primaryHouses = Houses.empty;
                   secondaryHouses = Houses.empty;
                   candidateReductions = CandidateReductions.empty;
                   setCellValueAction = Some setCellValue;
                   pointers = CandidateReductions.empty;
                   focus = Digits.empty }
        else None)
    |> List.choose id

let nakedN (i : int) (p : puzzleMap) (cellCandidates : cellCandidates) : hintDescription list =
    p.houses
    |> List.map (nakedNPerHouse i p cellCandidates )
    |> List.concat
