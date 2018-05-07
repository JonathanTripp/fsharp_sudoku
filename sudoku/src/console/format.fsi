module console.Format

open core.Sudoku
open core.Puzzlemap

type basic_color =
    | DefaultColour
    | Black
    | Red
    | Green
    | Yellow
    | Blue
    | Magenta
    | Cyan
    | White

(* Things we may want to write *)
[<NoComparison;NoEquality>]
type consoleChar = 
    | CNil
    | CChar of char
    | CStr of string
    | CDigit of digit
    | ColouredString of string * basic_color * basic_color
    | ColouredDigit of digit * basic_color * basic_color
    | NL

type consoleString = consoleChar list

(* Printing a row, we need special characters at left, in the middle and on the right *)
[<NoComparison;NoEquality>]
type gridCharsRow = 
    { l : consoleString;
      m : consoleString;
      r : consoleString }

(* Printing a grid, we need special rows at top, in the middle and on the bottom
 Also, horizontal and vertical spacers *)
 [<NoComparison;NoEquality>]
type gridChars = 
    { h : consoleString;
      v : gridCharsRow;
      t : gridCharsRow;
      m : gridCharsRow;
      b : gridCharsRow;
      n : consoleString }

[<NoComparison;NoEquality>]
type candidateGridCharsRow = 
    { mi : consoleString;
      x : gridCharsRow }

[<NoComparison;NoEquality>]
type candidateGridChars = 
    { h : consoleString;
      hi : consoleString;
      v : gridCharsRow;
      vi : consoleString;
      t : candidateGridCharsRow;
      m : candidateGridCharsRow;
      mi : candidateGridCharsRow;
      b : candidateGridCharsRow;
      n : consoleString }

val printLine : cells -> (cell -> consoleString) -> consoleString

val printGrid : puzzleMap -> gridChars -> (cell -> consoleString) -> consoleString

val printCandidateGrid : puzzleMap -> candidateGridChars -> digits -> (cell -> digit -> consoleString) -> consoleString
