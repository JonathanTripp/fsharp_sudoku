module core.EliminateCandidate

open Sudoku
open oset

let apply (p : Puzzlemap.puzzleMap) (candidate : candidate) (current : current) : current = 

    let update (cell : cell) : cellContents = 
        let cellContents = Current.get cell current
        match cellContents with
        | BigNumber _ -> cellContents
        | PencilMarks candidates -> 
            if candidate.cell = cell then PencilMarks(Digits.remove candidate.digit candidates)
            else cellContents

    Cells.ofLookup update p.cells
    |> Current.make

let description (p: Puzzlemap.puzzleMap) (candidate : candidate) : Hint.description =
    let cr = CandidateReduction.make (candidate.cell) (Digits.singleton candidate.digit) in

    { primaryHouses = Houses.empty;
      secondaryHouses = Houses.empty;
      candidateReductions = OSet.singleton cr;
      setCellValueAction = None;
      pointers = OSet.empty;
      focus = Digits.empty }

let step (p : Puzzlemap.puzzleMap) (candidate : candidate) (solution : solution) : solution =
    { solution with current = apply p candidate solution.current;
                    steps = (Eliminate candidate) :: solution.steps }
