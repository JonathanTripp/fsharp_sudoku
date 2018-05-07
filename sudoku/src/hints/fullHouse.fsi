module hints.FullHouse

open core.Sudoku
open compat.oset

val find : core.Puzzlemap.puzzleMap -> cellCandidates -> core.Hint.description list
