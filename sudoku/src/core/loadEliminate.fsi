module core.LoadEliminate

open Sudoku
open oset

val find : Puzzlemap.puzzleMap -> current -> candidateReduction list
val apply : Puzzlemap.puzzleMap -> candidateReduction list -> current -> current
val description : Puzzlemap.puzzleMap -> candidateReduction list -> Hint.description
val step : Puzzlemap.puzzleMap -> solution -> candidateReduction list -> solution
val findAndApply : Puzzlemap.puzzleMap -> solution -> solution
