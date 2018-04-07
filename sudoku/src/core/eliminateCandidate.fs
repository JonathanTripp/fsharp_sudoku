module core.EliminateCandidate

open Sudoku
open oset
open smap

let apply (p : Puzzlemap.puzzleMap) (candidate : candidate) (current : current) : current = 

    let update (cell : cell) : cellContents = 
        let cellContents = SMap.get cell current
        match cellContents with
        | BigNumber _ -> cellContents
        | PencilMarks candidates -> 
            if candidate.cell = cell then PencilMarks(OSet.remove candidate.digit candidates)
            else cellContents

    SMap.ofLookup update p.cells

let description (p: Puzzlemap.puzzleMap) (candidate : candidate) : Hint.description =
    let cr = CandidateReduction.make (candidate.cell) (OSet.singleton candidate.digit) in

    { primaryHouses = OSet.empty();
      secondaryHouses = OSet.empty();
      candidateReductions = OSet.singleton cr;
      setCellValueAction = None;
      pointers = OSet.empty();
      focus = OSet.empty() }

let step (p : Puzzlemap.puzzleMap) (candidate : candidate) (solution : solution) : solution =
    { solution with current = apply p candidate solution.current;
                    steps = (Eliminate candidate) :: solution.steps }
