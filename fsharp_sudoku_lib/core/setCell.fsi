module Sudoku.Lib.core.SetCell

open Sudoku.Lib.core.Sudoku

val apply : Puzzlemap.puzzleMap -> value -> current -> current

val try' : cell -> digit -> cellCandidates -> value option

val description : Puzzlemap.puzzleMap -> value -> Hint.description

val step : Puzzlemap.puzzleMap -> value -> solution -> solution
