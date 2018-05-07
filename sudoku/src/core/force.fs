module core.Force

open compat.oset
open compat.smap
open Sudoku

let isPencilMarksCellContents (cellContents : cellContents) : bool =
    match cellContents with
    | BigNumber _ -> false
    | PencilMarks _ -> true

let isValidCellContents (cellContents : cellContents) : bool =
    match cellContents with
    | BigNumber _ -> true
    | PencilMarks candidates -> OSet.count candidates > 0

let isValid (solution : solution) (cells : cells) : bool =
    cells
    |> OSet.map (fun cell -> SMap.get cell solution.current)
    |> OSet.forall isValidCellContents

let rec searchr (p : Puzzlemap.puzzleMap) (solution : solution) (existing : solution list) : solution list =
    let emptyCell : cell option =
        let is_cell_empty (cell : cell) : bool =
            SMap.get cell solution.current
            |> isPencilMarksCellContents
            in

        if OSet.exists is_cell_empty p.cells then
            Some (OSet.find is_cell_empty p.cells)
        else None
        in

    match emptyCell with
    | Some cell ->
        let candidates =
            let cellContents = SMap.get cell solution.current in
            match cellContents with
            | BigNumber _ -> OSet.empty()
            | PencilMarks candidates -> candidates
            in

        candidates
        |> OSet.toList
        |> List.map
            (fun digit ->
                let setCellValue = Value.make cell digit in
                
                let current = SetCell.apply p setCellValue solution.current in

                let newSolution =
                    { solution with
                        current = current;
                        steps = (Placement setCellValue) :: solution.steps }
                    in

                (*Console.WriteLine ("Trying {0}", setCellValue) *)

                if isValid newSolution p.cells then
                    (*Console.WriteLine(">")*)
                    searchr p newSolution existing
                else
                    (*
                    let cell =
                        List.find
                            (fun cell -> 
                                let cellContents = newSolution.current cell
                                match cellContents with
                                | BigNumber _ -> false
                                | PencilMarks candidates -> OSet.count candidates = 0)
                            cells

                    Console.WriteLine(String.Format("< {0}", cell))
                    *)
                    [])
            |> List.concat
    | None -> solution :: existing

let solve (p : Puzzlemap.puzzleMap) (solution : solution) : solution list =
    searchr p solution []
