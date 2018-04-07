module hints.Wing

open core.Sudoku
open oset

val xWings : core.Puzzlemap.puzzleMap -> cellCandidates -> core.Hint.description list

val yWings : core.Puzzlemap.puzzleMap -> cellCandidates -> core.Hint.description list
