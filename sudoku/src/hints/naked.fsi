module hints.Naked

open core.Sudoku
open compat.oset

val find : int -> core.Puzzlemap.puzzleMap -> cellCandidates -> core.Hint.description list
