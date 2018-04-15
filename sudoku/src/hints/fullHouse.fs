module hints.FullHouse

(* Full House means:
 For a house there is only one cell that is neither given nor set i.e. has candidates *)

open core.Sudoku
open oset

let fullHousePerHouse (p : core.Puzzlemap.puzzleMap) (cellCandidates : cellCandidates) (primaryHouse : house) : core.Hint.description option =

    let hhs =
        p.houseCellCandidateReductions primaryHouse cellCandidates
        |> List.filter (fun cr -> OSet.count cr.candidates > 0) 
        in

    if List.length hhs = 1 then 
        let h = List.head hhs in
        let cell = h.cell in
        let candidate = OSet.head h.candidates in

        let setCellValue = Value.make cell candidate in

        Some { primaryHouses = OSet.singleton primaryHouse;
               secondaryHouses = OSet.empty();
               candidateReductions = [];
               setCellValueAction = Some setCellValue;
               pointers = [];
               focus = OSet.empty() }
    else None

let find (p : core.Puzzlemap.puzzleMap) (cellCandidates : cellCandidates) : core.Hint.description list =
    p.houses
    |> OSet.toList
    |> List.choose (fullHousePerHouse p cellCandidates)
