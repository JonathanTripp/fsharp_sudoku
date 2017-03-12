module console.Format

open core.Sudoku

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
type gridCharsRow = 
    { l : consoleString;
      m : consoleString;
      r : consoleString }

(* Printing a grid, we need special rows at top, in the middle and on the bottom
 Also, horizontal and vertical spacers *)
type gridChars = 
    { h : consoleString;
      v : gridCharsRow;
      t : gridCharsRow;
      m : gridCharsRow;
      b : gridCharsRow;
      n : consoleString }

type candidateGridCharsRow = 
    { mi : consoleString;
      x : gridCharsRow }

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

val printGrid : core.Puzzlemap.puzzleMap -> gridChars -> (cell -> consoleString) -> consoleString

val printCandidateGrid : core.Puzzlemap.puzzleMap -> candidateGridChars -> digits -> (cell -> digit -> consoleString) -> consoleString
