module hints.FullHouse

open core.Sudoku
open oset

val find : core.Puzzlemap.puzzleMap -> cellCandidates -> core.Hint.description list
