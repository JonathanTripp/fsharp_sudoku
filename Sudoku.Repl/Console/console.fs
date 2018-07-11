module Sudoku.Repl.console.Console

open Sudoku.Lib.compat.Sset
open Sudoku.Lib.compat.oset
open Sudoku.Lib.compat.smap

open Sudoku.Lib.core.Sudoku
open Sudoku.Lib.core.Hint

let drawDigitCellContents (given : digit option) (current : cellContents) : Format.consoleChar = 
    match given, current with
    | Some s, _ -> Format.ColouredDigit(s, Format.Blue, Format.DefaultColour)
    | None, BigNumber s -> Format.ColouredDigit(s, Format.Red, Format.DefaultColour)
    | None, PencilMarks _ -> Format.CChar '.'

let drawDigitCellString (given : digit option) (current : cellContents) : Format.consoleString =
    [drawDigitCellContents given current]

let drawBigNumber (annotation : annotation) (digit : digit) : Format.consoleChar =
    if annotation.primaryHintHouse then
        match annotation.given with
        | Some _ -> Format.ColouredDigit(digit, Format.Cyan, Format.DefaultColour)
        | None -> Format.ColouredDigit(digit, Format.Yellow, Format.DefaultColour)
    else if annotation.secondaryHintHouse then
        match annotation.given with
        | Some _ -> Format.ColouredDigit(digit, Format.DefaultColour, Format.Blue)
        | None -> Format.ColouredDigit(digit, Format.DefaultColour, Format.Red)
    else
        match annotation.given with
        | Some _ -> Format.ColouredDigit(digit, Format.Blue, Format.White)
        | None -> Format.ColouredDigit(digit, Format.Red, Format.DefaultColour)

let drawPencilMarks (annotation : annotation) (candidate : digit) (candidates : digits) : Format.consoleChar =
    match annotation.setValue with
    | Some vv when Digit.setElemCompare vv candidate = EQ -> 
        Format.ColouredDigit(candidate, Format.Red, Format.DefaultColour)
    | Some _ when OSet.contains candidate candidates -> 
        Format.ColouredDigit(candidate, Format.Green, Format.DefaultColour)
    | _ ->
        (match annotation.setValueReduction with
         | Some svr when Digit.setElemCompare svr candidate = EQ && OSet.contains candidate candidates -> 
            Format.ColouredDigit(candidate, Format.Green, Format.DefaultColour)
         | _ ->
            (if OSet.contains candidate annotation.reductions then
                Format.ColouredDigit(candidate, Format.Green, Format.DefaultColour)
             else if OSet.contains candidate annotation.pointers then
                Format.ColouredDigit(candidate, Format.Magenta, Format.DefaultColour)
             else if OSet.contains candidate annotation.focus && OSet.contains candidate candidates then
                Format.ColouredDigit(candidate, Format.Yellow, Format.DefaultColour)
             else if annotation.primaryHintHouse then
                if OSet.contains candidate candidates then Format.ColouredDigit(candidate, Format.Cyan, Format.DefaultColour)
                else Format.CChar ' '
             else if annotation.secondaryHintHouse then
                if OSet.contains candidate candidates then Format.ColouredDigit(candidate, Format.Green, Format.DefaultColour)
                else Format.CChar ' '
             else
                if OSet.contains candidate candidates then Format.ColouredDigit(candidate, Format.Green, Format.DefaultColour)
                else Format.CChar ' '))

let drawDigitCellContentAnnotations centreCandidate (annotations : SMap<cell, annotation>) (cell : cell) (candidate : digit) : Format.consoleChar = 

    let annotation = SMap.get cell annotations in

    match annotation.current with
    | BigNumber s when Digit.setElemCompare centreCandidate candidate = EQ -> drawBigNumber annotation s
    | BigNumber _ -> Format.ColouredString(" ", Format.Blue, Format.White)
    | PencilMarks digits -> drawPencilMarks annotation candidate digits

let drawDigitCellContentAnnotationString (centreCandidate : digit) (annotations : SMap<cell, annotation>) (cell : cell) (candidate : digit) : Format.consoleString =
    [drawDigitCellContentAnnotations centreCandidate annotations cell candidate]
