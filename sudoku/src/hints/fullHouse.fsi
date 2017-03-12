module hints.FullHouse

open core.Sudoku

val find : core.Puzzlemap.puzzleMap -> cellCandidates -> core.Hint.description list
