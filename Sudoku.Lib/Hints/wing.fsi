module Sudoku.Lib.hints.Wing

open Sudoku.Lib.core.Sudoku
open Sudoku.Lib.core.Puzzlemap
open Sudoku.Lib.core.Hint

val xWings : puzzleMap -> cellCandidates -> descriptions

val yWings : puzzleMap -> cellCandidates -> descriptions
