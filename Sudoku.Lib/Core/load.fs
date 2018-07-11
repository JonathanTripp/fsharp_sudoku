module Sudoku.Lib.core.Load

open Sudoku.Lib.compat
open Sudoku.Lib.compat.oset
open Sudoku.Lib.compat.smap
open Sudoku.Lib.core.Sudoku

(* Load a sudoku given as a single line of gridSize*gridSize characters *)
let loadPuzzle (cells : cells) (alphabetisedLine : digit option list) : given =
    Sset.zip (OSet.toList cells) alphabetisedLine |> SMap.ofList

let load (puzzleShape : puzzleShape) (sudoku : string) : solution = 

    let charToDigit (trialDigit : char) : digit option = 
        let compareAlpha (Digit charDigit) = trialDigit = charDigit in
        let digits = puzzleShape.alphabet in
        if OSet.exists compareAlpha digits then
            Some (OSet.find compareAlpha digits)
        else None
        in

    let alphabetisedLine : digit option list =
        sudoku
        |> Sset.explode
        |> List.map charToDigit
        in

    let p = Puzzlemap.tPuzzleMap puzzleShape in

    let given = loadPuzzle p.cells alphabetisedLine in

    let current = Solution.givenToCurrent p.cells given puzzleShape.alphabet in

    { given = given;
      current = current;
      steps = [ Load sudoku ] }
