module Sudoku.Lib.hints.Hidden

open Sudoku.Lib.core.Sudoku
open Sudoku.Lib.core.Puzzlemap
open Sudoku.Lib.core.Hint

val find : int -> puzzleMap -> cellCandidates -> descriptions
