module hints.FullHouse

open core.Sudoku
open core.Puzzlemap
open core.Hint

val find : puzzleMap -> cellCandidates -> descriptions
