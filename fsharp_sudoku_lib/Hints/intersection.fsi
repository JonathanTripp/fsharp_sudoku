module Sudoku.Lib.hints.Intersection

open Sudoku.Lib.core.Sudoku
open Sudoku.Lib.core.Puzzlemap
open Sudoku.Lib.core.Hint

val pointingPairs : puzzleMap -> cellCandidates -> descriptions

val boxLineReductions : puzzleMap -> cellCandidates -> descriptions
