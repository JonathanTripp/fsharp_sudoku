module core.SetCell

open Sudoku

val apply : Puzzlemap.puzzleMap -> value -> current -> current

val try' : cell -> digit -> cellCandidates -> value option

val description : Puzzlemap.puzzleMap -> value -> Hint.description

val step : Puzzlemap.puzzleMap -> value -> solution -> solution
