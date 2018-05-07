module hints.Wing

open core.Sudoku
open core.Puzzlemap
open core.Hint

val xWings : puzzleMap -> cellCandidates -> descriptions

val yWings : puzzleMap -> cellCandidates -> descriptions
