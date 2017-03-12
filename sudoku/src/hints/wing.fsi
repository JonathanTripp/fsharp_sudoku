module hints.Wing

open core.Sudoku

val xWings : core.Puzzlemap.puzzleMap -> cellCandidates -> core.Hint.description list

val yWings : core.Puzzlemap.puzzleMap -> cellCandidates -> core.Hint.description list
