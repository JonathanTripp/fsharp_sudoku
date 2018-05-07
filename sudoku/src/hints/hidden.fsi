module hints.Hidden

open core.Sudoku
open core.Puzzlemap
open core.Hint

val find : int -> puzzleMap -> cellCandidates -> descriptions
