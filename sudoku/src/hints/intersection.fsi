module hints.Intersection

open core.Sudoku
open oset

val pointingPairs : core.Puzzlemap.puzzleMap -> cellCandidates -> OSet<core.Hint.description>

val boxLineReductions : core.Puzzlemap.puzzleMap -> cellCandidates -> OSet<core.Hint.description>
