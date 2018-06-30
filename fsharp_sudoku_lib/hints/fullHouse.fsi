module Sudoku.Lib.hints.FullHouse

open Sudoku.Lib.core.Sudoku
open Sudoku.Lib.core.Puzzlemap
open Sudoku.Lib.core.Hint

val find : puzzleMap -> cellCandidates -> descriptions
