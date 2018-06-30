module Sudoku.Lib.core.LoadEliminate

open Sudoku.Lib.core.Sudoku

val find : Puzzlemap.puzzleMap -> current -> candidateReductions
val apply : Puzzlemap.puzzleMap -> candidateReductions -> current -> current
val description : Puzzlemap.puzzleMap -> candidateReductions -> Hint.description
val step : Puzzlemap.puzzleMap -> solution -> candidateReductions -> solution
val findAndApply : Puzzlemap.puzzleMap -> solution -> solution
