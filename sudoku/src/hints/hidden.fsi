module hints.Hidden

open core.Sudoku

val find : int -> core.Puzzlemap.puzzleMap -> cellCandidates -> core.Hint.description list
