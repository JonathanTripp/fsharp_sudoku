module Sudoku.Repl.console.Console

open Sudoku.Lib.compat.smap

open Sudoku.Lib.core.Sudoku
open Sudoku.Lib.core.Hint

val drawDigitCellString : digit option -> cellContents -> Format.consoleString

val drawDigitCellContentAnnotationString : digit -> SMap<cell, annotation> -> cell -> digit -> Format.consoleString
