module input.Load

open core.Sudoku
open oset

(* Load a sudoku given as a single line of gridSize*gridSize characters *)
let loadPuzzle (cells : cells) (alphabetisedLine : digit option list) : given =
    Given (Sset.zip (OSet.toList cells) alphabetisedLine)

let load (puzzleShape : puzzleShape) (sudoku : string) : solution = 

    let charToDigit (trialDigit : char) : digit option = 
        let compareAlpha (Digit charDigit) = trialDigit = charDigit in
        let digits = Digits.to_list puzzleShape.alphabet in
        if List.exists compareAlpha digits then
            Some (List.find compareAlpha digits)
        else None
        in

    let alphabetisedLine : digit option list =
        sudoku
        |> Sset.explode
        |> List.map charToDigit
        in

    let p = core.Puzzlemap.tPuzzleMap puzzleShape in

    let given = loadPuzzle p.cells alphabetisedLine in

    let current = Solution.givenToCurrent p.cells given puzzleShape.alphabet in

    { given = given;
      current = current;
      steps = [ Load sudoku ] }
