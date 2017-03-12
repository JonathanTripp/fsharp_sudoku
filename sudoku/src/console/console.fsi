module console.Console

open core.Sudoku

val drawDigitCellString : digit option -> cellContents -> Format.consoleString

val drawDigitCellContentAnnotationString : digit -> (cell * core.Hint.annotation) list -> cell -> digit -> Format.consoleString
