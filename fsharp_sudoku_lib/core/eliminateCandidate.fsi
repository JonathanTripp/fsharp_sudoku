module core.EliminateCandidate

open Sudoku

val description : Puzzlemap.puzzleMap -> candidate -> Hint.description

val step : Puzzlemap.puzzleMap -> candidate -> solution -> solution
