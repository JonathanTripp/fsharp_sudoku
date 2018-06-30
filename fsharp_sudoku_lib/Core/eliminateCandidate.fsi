module Sudoku.Lib.core.EliminateCandidate

open Sudoku.Lib.core.Sudoku

val description : Puzzlemap.puzzleMap -> candidate -> Hint.description

val step : Puzzlemap.puzzleMap -> candidate -> solution -> solution
