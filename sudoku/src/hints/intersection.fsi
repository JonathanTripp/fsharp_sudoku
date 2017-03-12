module hints.Intersection

open core.Sudoku

val pointingPairs : core.Puzzlemap.puzzleMap -> cellCandidates -> core.Hint.description list

val boxLineReductions : core.Puzzlemap.puzzleMap -> cellCandidates -> core.Hint.description list
