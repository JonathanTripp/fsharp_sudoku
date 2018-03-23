module console.Console

open core.Sudoku
open smap

val drawDigitCellString : digit option -> cellContents -> Format.consoleString

val drawDigitCellContentAnnotationString : digit -> SMap<cell, core.Hint.annotation> -> cell -> digit -> Format.consoleString
