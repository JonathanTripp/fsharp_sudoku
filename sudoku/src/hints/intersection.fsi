module hints.Intersection

open core.Sudoku
open compat.oset

val pointingPairs : core.Puzzlemap.puzzleMap -> cellCandidates -> core.Hint.description list

val boxLineReductions : core.Puzzlemap.puzzleMap -> cellCandidates -> core.Hint.description list
