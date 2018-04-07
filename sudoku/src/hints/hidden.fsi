module hints.Hidden

open core.Sudoku
open oset

val find : int -> core.Puzzlemap.puzzleMap -> cellCandidates -> core.Hint.description list
