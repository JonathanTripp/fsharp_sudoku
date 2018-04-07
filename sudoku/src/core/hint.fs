module core.Hint

open Sudoku
open oset
open smap

exception CellStateInvalid

type description = 
    { primaryHouses : houses;
      secondaryHouses : houses;
      candidateReductions : OSet<candidateReduction>;
      setCellValueAction : value option;
      pointers : OSet<candidateReduction>;
      focus : digits }

module Description =
    let to_string (h : description) : string =

        let line1 = Printf.sprintf "Primary OSet %s\r\n" (Houses.toString h.primaryHouses) in
        let line2 = Printf.sprintf "Secondary OSet %s\r\n" (Houses.toString h.secondaryHouses) in
        let line3 = Printf.sprintf "Pointers %s\r\n" (CandidateReductions.to_string (h.pointers |> OSet.toList)) in

        let crlines =
            h.candidateReductions
            |> OSet.toList
            |> List.map
                (fun candidateReduction ->
                    Printf.sprintf "  %s\r\n" (CandidateReduction.to_string candidateReduction))
            in

        [ line1; line2; line3; String.concat "," crlines]
        |> String.concat ","

(* To draw a cell we may want to display extra information... *)
type annotation = 
    { given : digit option;
      current: cellContents;
      setValue : digit option;
      primaryHintHouse : bool;
      secondaryHintHouse : bool;
      setValueReduction : digit option;
      reductions : digits;
      pointers : digits;
      focus : digits }

type description2 = 
    { annotations : SMap<cell, annotation> }

let mhas (solution : solution) (p : Puzzlemap.puzzleMap) (hd : description) : description2 = 

    let annotationLookup (cell : cell) : annotation = 

        let setValue, setValueReduction = 
            match hd.setCellValueAction with
            | Some setCellValueAction -> 
                
                let r1 = 
                    if setCellValueAction.cell = cell then Some setCellValueAction.digit
                    else None
                    in

                let r3 = 
                    let cells = SMap.get setCellValueAction.cell p.cellHouseCells in

                    if OSet.contains cell cells then Some setCellValueAction.digit
                    else None
                    in

                r1, r3
            | None -> None, None
            in

        let cellCandidateReductions =
            hd.candidateReductions
            |> OSet.filter (fun pointer -> cell = pointer.cell) 
            in

        let reductions = 
            match (cellCandidateReductions |> OSet.toList) with
            | cr :: _ -> cr.candidates
            | [] -> OSet.empty()
            in

        let cellPointers =
            hd.pointers
            |> OSet.filter (fun pointer -> cell = pointer.cell)
            in

        let pointers = 
            match (cellPointers |> OSet.toList) with
            | cr :: _ -> cr.candidates
            | [] -> OSet.empty()
            in

        let primaryHouseCells =
            p.housesCells hd.primaryHouses
            in

        let secondaryHouseCells =
            p.housesCells hd.secondaryHouses
            in

        { given = SMap.get cell solution.given;
          current = SMap.get cell solution.current;
          setValue = setValue;
          primaryHintHouse = OSet.contains cell primaryHouseCells;
          secondaryHintHouse = OSet.contains cell secondaryHouseCells;
          setValueReduction = setValueReduction;
          reductions = reductions;
          pointers = pointers;
          focus = hd.focus }
        in

    let annotations = SMap.ofLookup annotationLookup p.cells in

    { annotations = annotations }

let mhas2 (solution : solution) (p : Puzzlemap.puzzleMap) : description2 = 

    let annotationLookup (cell : cell) : annotation = 
        { given = SMap.get cell solution.given;
          current = SMap.get cell solution.current;
          setValue = None;
          primaryHintHouse = false;
          secondaryHintHouse = false;
          setValueReduction = None;
          reductions = OSet.empty();
          pointers = OSet.empty();
          focus = OSet.empty() }
        in

    let annotations = SMap.ofLookup annotationLookup p.cells in

    { annotations = annotations }
