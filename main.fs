﻿module main

open System
open System.Diagnostics
open System.Runtime.InteropServices
open System.Text

open core.sudoku
open core.puzzlemap
open core.hints
open core.setCell
open core.eliminateCandidate
open core.force

open console.command
open console.console
open console.format

open load

[<DllImport("user32.dll")>]
extern bool ShowWindow(System.IntPtr hWnd, int cmdShow)

let Maximize() = 
    let p = Process.GetCurrentProcess()
    ShowWindow(p.MainWindowHandle, 3) //SW_MAXIMIZE = 3

let parse (item : string) (solution : solution) (puzzle : puzzleShape) 
    (candidateLookup : cellCandidates) puzzleDrawFull2 print_last : solution * Set<hintDescription> = 

    let p = tPuzzleMap puzzle :> puzzleMap

    Console.WriteLine item

    if item = "print" then 
        puzzleDrawFull2 None
        (solution, Set.empty)
    else if item.StartsWith "focus" then
        let focusDigitOpt = focusCommandParse puzzle item
        match focusDigitOpt with
        | Some focusDigit ->
            let hd2 = focusCommandHintDescription p focusDigit
            let hd3 = mhas solution p hd2
            puzzleDrawFull2 (Some hd3.annotations)

            (solution, Set.empty)
        | None ->
            (solution, Set.empty)

    else if item.StartsWith "s" then
        let valueOpt = setCellCommandParse puzzle item p
        
        let newSolution = 
            match valueOpt with
            | Some value ->
                let setCellValueOpt = setCellCommandCheck solution.given candidateLookup value
                match setCellValueOpt with
                | Some setCellValue ->
                    let hd2 = setCellHintDescription p setCellValue
                    let hd3 = mhas solution p hd2
                    puzzleDrawFull2 (Some hd3.annotations)

                    setCellStep p setCellValue solution

                | None ->
                    Console.WriteLine "Expect set <col> <row> <val>"
                    solution

            | None -> 
                Console.WriteLine "Expect set <col> <row> <val>"
                solution

        print_last newSolution
        (newSolution, Set.empty)

    else if item.StartsWith "c" then
        let candidateOpt = candidateClearCommandParse puzzle item p

        let newSolution = 
            match candidateOpt with
            | Some candidate ->
                let clearCommandOpt = candidateClearCommandCheck solution.given candidateLookup candidate
                match clearCommandOpt with
                | Some clearCommand ->
                    let hd2 = eliminateCandidateHintDescription p candidate
                    let hd3 = mhas solution p hd2
                    puzzleDrawFull2 (Some hd3.annotations)

                    eliminateCandidateStep p candidate solution

                | None -> 
                    Console.WriteLine "Expect clr <col> <row> <val>"
                    solution

            | None -> 
                Console.WriteLine "Expect clr <col> <row> <val>"
                solution

        print_last newSolution
        (newSolution, Set.empty)

    else
        let supportedHintOpt = SupportedHints.TryFind item
        match supportedHintOpt with
        | Some supportedHint ->
            let hints = supportedHint p candidateLookup
            (solution, hints)
        | None -> (solution, Set.empty)

let printHint (solution : solution) (p : puzzleMap) drawHint (index : int) (hint : hintDescription) : unit = 

    Console.WriteLine("{0}: {1}", index, hint)

    let hd3 = mhas solution p hint
    drawHint (Some hd3.annotations)

    Console.Read() |> ignore

let run (solution : solution ref) (puzzle : puzzleShape) 
    puzzleDrawCandidateGridAnnotations print_last puzzlePrintHint item = 
    if item = "quit" then Some "quit"
    else
        let mapCandidateCandidates = currentCellCandidates (!solution).current

        let (soln, hints) = 
            parse item !solution puzzle mapCandidateCandidates puzzleDrawCandidateGridAnnotations print_last
        solution := soln

        Seq.iteri puzzlePrintHint hints

        None

let digitToEntry (cellDigitLookup : given) (alphabet : Set<digit>) 
    (cellHouseCells : cellHouseCells) : current = 
    let m (cell : cell) (dop : digit option) : cellContents =
        match dop with
        | Some(e) -> BigNumber(e)
        | None -> 
            let cells =
                cell
                |> cellHouseCells.Get
                |> Set.toList
            let digits =
                cells
                |> List.choose (fun cell -> cellDigitLookup.Item cell)
                |> Set.ofList

            Set.difference alphabet digits
            |> PencilMarks
    cellDigitLookup
    |> Map.map m

let repl (sudoku : string) (puzzle : puzzleShape) = 

    Console.WriteLine sudoku
    
    let p = tPuzzleMap puzzle :> puzzleMap

    let transformer (puzzleGrid : given) : current = 
        digitToEntry puzzleGrid (Set.ofList puzzle.alphabet) p.cellHouseCells
    
    let solution = ref (load p.orderedCells puzzle.alphabet (List.ofSeq sudoku) transformer)

    let centreDigit : digit = List.nth puzzle.alphabet ((List.length puzzle.alphabet) / 2)

    let puzzlePrintLine = printLine p.orderedCells

    let puzzlePrintGrid = printGrid p.orderedStacks p.orderedStackColumns p.orderedBands p.orderedBandRows defaultGridChars

    let puzzlePrintCandidateGrid = 
        printCandidateGrid p.orderedStacks p.orderedStackColumns p.orderedBands p.orderedBandRows defaultCandidateGridChars 
            puzzle.alphabet

    // Print a Digit option, with colours
    let puzzleDrawCell (solution : solution) (cell : cell) : consoleChar = 
        drawDigitCellContents (solution.given.Item cell) (solution.current.Item cell)

    let puzzleDrawCellDigit (solution : solution) (cell : cell) (digit : digit) : consoleChar = 
        drawDigitCellContentAnnotations centreDigit digit (solution.given.Item cell) 
            (solution.current.Item cell) None

    let puzzleDrawCellDigitAnnotations (solution : solution) (l : cellAnnotations option) (cell : cell) (digit : digit) : consoleChar = 
        let cellAnnotationOpt =
            l
            |> Option.map (fun ll -> ll.Item cell)

        drawDigitCellContentAnnotations centreDigit digit (solution.given.Item cell) 
            (solution.current.Item cell) cellAnnotationOpt

    let puzzleDrawLine () =
        Seq.iter drawConsoleChar (puzzlePrintLine (puzzleDrawCell !solution))

    let puzzleDrawGrid () =
        Seq.iter drawConsoleChar (puzzlePrintGrid (puzzleDrawCell !solution))
    
    let puzzleDrawCandidateGridAnnotations annotations = 
        Seq.iter drawConsoleChar (puzzlePrintCandidateGrid (puzzleDrawCellDigitAnnotations !solution annotations))

    let print_last (solution : solution) = 
        let puzzleDrawCell' (cell : cell) = drawDigitCellContents (solution.given.Item cell) (solution.current.Item cell)
        Seq.iter drawConsoleChar (puzzlePrintGrid puzzleDrawCell')

        match solution.steps with
        | action :: _ -> 
            match action with
            | Placement sv -> drawConsoleChar (CStr(sv.ToString()))
            | Eliminate candidate -> drawConsoleChar (CStr(candidate.ToString()))
        | [] -> ()

        drawConsoleChar NL

    let getInput (prompt : string) = 
        Console.Write prompt
        Console.ReadLine()
    
    let readlines = Seq.initInfinite (fun _ -> getInput (">"))

    puzzleDrawLine()
    drawConsoleChar NL

    puzzleDrawGrid()

    //let forcedSolutions = solve (!solution) p.cells p.cellHouseCells
    //puzzleDrawGrid()
    //if List.length forcedSolutions > 0 then
    //    List.iter
    //        (fun solve -> Seq.iter drawConsoleChar (puzzlePrintGrid (puzzleDrawCell solve)))
    //        forcedSolutions
    //else Console.WriteLine("No solutions")

    let puzzlePrintHint = printHint (!solution) p puzzleDrawCandidateGridAnnotations

    Seq.tryPick (run solution puzzle puzzleDrawCandidateGridAnnotations print_last puzzlePrintHint) 
        readlines |> ignore

let defaultPuzzleSpec = 
    { size = 9
      boxWidth = 3
      boxHeight = 3
      alphabet = 
          [ for i in 1..9 -> (char) i + '0'
                             |> Digit ] }

(*
let defaultPuzzleSpec = {
    boxWidth = 4 * 1<width>
    boxHeight = 2 * 1<height>
    alphabet = [ for i in 1 .. 8 -> (char) i + '0' |> Digit ]
    digits = fun _ -> None
}
*)

Maximize() |> ignore

// Input puzzle
Console.WriteLine "1........2........3........4........5........6........7........8........9........"
Console.WriteLine "123456789123456789123456789123456789123456789123456789123456789123456789123456789"

//let example = "410230000700580040000000020190000700380000016000008400000806005031050000000090800"
let example = "000105000140000670080002400063070010900000003010090520007200080026000035000409000"

// FullHouse
//let example = "800739006370465000040182009000600040054300610060500000400853070000271064100940002"
//let example = "801006094300009080970080500547062030632000050198375246083620915065198000219500008"
//let example = "2...3..7.9...1..8.5...6.9.4653871492489325761721496.....5.8.....6..4.....9..5...3"

// ht
//let example = "528600049136490025794205630000100200007826300002509060240300976809702413070904582"
// hq
//let example = "...3742......82.4..............3.8266...9...48.5.4697.547.2...9......4.5.1.45.7.2"
// http://www.sudokuwiki.org/Hidden_Candidates hq
//let example = "65..87.24...649.5..4..25...57.438.61...5.1...31.9.2.85...89..1....213...13.75..98"
//let example = "000500000425090001800010020500000000019000460000000002090040003200060807000001600"

// http://www.sudokuwiki.org/X_Wing_Strategy
//let example = "100000569492056108056109240009640801064010000218035604040500016905061402621000005"
// http://www.sudokuwiki.org/Y_Wing_Strategy
//let example = "900240000050690231020050090090700320002935607070002900069020073510079062207086009"
//let example = "273005081810302004009010200100953728792186345538724196021060500300201869080530412"


repl example defaultPuzzleSpec

Console.WriteLine "bye"
