module core.LoadEliminate

open Sudoku
open oset

val find : Puzzlemap.puzzleMap -> current -> OSet<candidateReduction>
val apply : Puzzlemap.puzzleMap -> OSet<candidateReduction> -> current -> current
val description : Puzzlemap.puzzleMap -> OSet<candidateReduction> -> Hint.description
val step : Puzzlemap.puzzleMap -> solution -> OSet<candidateReduction> -> solution
val findAndApply : Puzzlemap.puzzleMap -> solution -> solution
