module hints.Wing

open core.Sudoku
open oset

val xWings : core.Puzzlemap.puzzleMap -> cellCandidates -> OSet<core.Hint.description>

val yWings : core.Puzzlemap.puzzleMap -> cellCandidates -> OSet<core.Hint.description>
