module Sudoku.Lib.core.EliminateCandidate

open Sudoku.Lib.compat.Sset
open Sudoku.Lib.compat.oset
open Sudoku.Lib.compat.smap

open Sudoku.Lib.core.Sudoku

let apply (p : Puzzlemap.puzzleMap) (candidate : candidate) (current : current) : current = 

    let update (cell : cell) : cellContents = 
        let cellContents = SMap.get cell current
        match cellContents with
        | BigNumber _ -> cellContents
        | PencilMarks candidates -> 
            if Cell.setElemCompare candidate.cell cell = EQ then PencilMarks(OSet.remove candidate.digit candidates)
            else cellContents

    SMap.ofLookup update p.cells

let description (p: Puzzlemap.puzzleMap) (candidate : candidate) : Hint.description =
    let cr = CandidateReduction.make (candidate.cell) (OSet.singleton candidate.digit) in

    { primaryHouses = OSet.empty();
      secondaryHouses = OSet.empty();
      candidateReductions = [cr];
      setCellValueAction = None;
      pointers = [];
      focus = OSet.empty() }

let step (p : Puzzlemap.puzzleMap) (candidate : candidate) (solution : solution) : solution =
    { solution with current = apply p candidate solution.current;
                    steps = (Eliminate candidate) :: solution.steps }
