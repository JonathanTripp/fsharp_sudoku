module console.Console

open core.Sudoku

let drawDigitCellContents (given : digit option) (current : cellContents) : Format.consoleChar = 
    match given, current with
    | Some s, _ -> Format.ColouredDigit(s, Format.Blue, Format.DefaultColour)
    | None, BigNumber s -> Format.ColouredDigit(s, Format.Red, Format.DefaultColour)
    | None, PencilMarks _ -> Format.CChar '.'

let drawDigitCellString (given : digit option) (current : cellContents) : Format.consoleString =
    [drawDigitCellContents given current]

let drawBigNumber (annotation : core.Hint.annotation) (digit : digit) : Format.consoleChar =
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

let drawPencilMarks (annotation : core.Hint.annotation) (candidate : digit) (candidates : digits) : Format.consoleChar =
    match annotation.setValue with
    | Some vv when vv = candidate -> 
        Format.ColouredDigit(candidate, Format.Red, Format.DefaultColour)
    | Some _ when Digits.contains candidate candidates -> 
        Format.ColouredDigit(candidate, Format.Green, Format.DefaultColour)
    | _ ->
        (match annotation.setValueReduction with
         | Some svr when svr = candidate && Digits.contains candidate candidates -> 
            Format.ColouredDigit(candidate, Format.Green, Format.DefaultColour)
         | _ ->
            (if Digits.contains candidate annotation.reductions then
                Format.ColouredDigit(candidate, Format.Green, Format.DefaultColour)
             else if Digits.contains candidate annotation.pointers then
                Format.ColouredDigit(candidate, Format.Magenta, Format.DefaultColour)
             else if Digits.contains candidate annotation.focus && Digits.contains candidate candidates then
                Format.ColouredDigit(candidate, Format.Yellow, Format.DefaultColour)
             else if annotation.primaryHintHouse then
                if Digits.contains candidate candidates then Format.ColouredDigit(candidate, Format.Cyan, Format.DefaultColour)
                else Format.CChar ' '
             else if annotation.secondaryHintHouse then
                if Digits.contains candidate candidates then Format.ColouredDigit(candidate, Format.Green, Format.DefaultColour)
                else Format.CChar ' '
             else
                if Digits.contains candidate candidates then Format.ColouredDigit(candidate, Format.Green, Format.DefaultColour)
                else Format.CChar ' '))

let drawDigitCellContentAnnotations centreCandidate (annotations : (cell * core.Hint.annotation) list) (cell : cell) (candidate : digit) : Format.consoleChar = 

    let annotation = Smap.get Cell.comparer cell annotations in

    match annotation.current with
    | BigNumber s when centreCandidate = candidate -> drawBigNumber annotation s
    | BigNumber _ -> Format.ColouredString(" ", Format.Blue, Format.White)
    | PencilMarks digits -> drawPencilMarks annotation candidate digits

let drawDigitCellContentAnnotationString (centreCandidate : digit) (annotations : (cell * core.Hint.annotation) list) (cell : cell) (candidate : digit) : Format.consoleString =
    [drawDigitCellContentAnnotations centreCandidate annotations cell candidate]
