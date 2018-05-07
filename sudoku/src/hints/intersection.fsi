module hints.Intersection

open core.Sudoku
open core.Puzzlemap
open core.Hint

val pointingPairs : puzzleMap -> cellCandidates -> descriptions

val boxLineReductions : puzzleMap -> cellCandidates -> descriptions
