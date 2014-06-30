﻿module format

open sudoku
open puzzlemap

// Printing a row, we need special characters at left, in the middle and on the right
type gridCharsRow<'a> = {
    l:'a
    m:'a
    r:'a
}

// Printing a grid, we need special rows at top, in the middle and on the bottom
// Also, horizontal and vertical spacers
type gridChars<'a> = {
    h:'a
    v:gridCharsRow<'a>

    t:gridCharsRow<'a>
    m:gridCharsRow<'a>
    b:gridCharsRow<'a>
}

type solutionCharsRow<'a> = {
    mi:'a
    x:gridCharsRow<'a>
}

type solutionChars<'a> = {
    h:'a
    hi:'a
    v:gridCharsRow<'a>
    vi:'a
    t:solutionCharsRow<'a>
    m:solutionCharsRow<'a>
    mi:solutionCharsRow<'a>
    b:solutionCharsRow<'a>
}

// Combine fences with posts (there's one more fence than posts: f p f p ... p f)
let simpleInterleave (fenceToSeq:'a->seq<'c>) (post:seq<'c>) (fences:'a list) =
    seq {
        match fences with
        | f :: fs ->
            yield! fenceToSeq f

            for fence in fs do
                yield! post
                yield! fenceToSeq fence

        | [] -> ()
    }

// Create a sequence of fences interleaved with posts (first and last posts may be different)
// l f p f p f ... p f r
let sinterleave (fenceToSeq:'a->seq<'c>) (firstPost:seq<'c>) (midPost:seq<'c>) (lastPost:seq<'c>) (eol:seq<'c>) (fences:'a list) =
    seq {
        yield! firstPost

        yield! simpleInterleave fenceToSeq midPost fences

        yield! lastPost
        yield! eol
    }

let printRowOnOneLine (symbolTo:'b->'c) (containerItems:'a->'b list) (midPost:seq<'c>) (containers:'a list) =
    let containerToSeq container = Seq.map symbolTo (containerItems container)

    sinterleave containerToSeq midPost midPost midPost Seq.empty containers

// Print a column
let printCell (symbolTo:Cell->'c) cell =
    symbolTo cell |> Seq.singleton

let printColumn (printCell:Cell->seq<'c>) row column =
    let cell = { Cell.col = column; row = row }
    printCell cell

// Print a stack
let printStack (columnPrinter:Row->Column->seq<'c>) (columnSeparator:seq<'c>) (stackColumns:Stack->Column list) row (stack:Stack) =
    simpleInterleave (columnPrinter row) columnSeparator (stackColumns stack)

// Print a row
let printRow (stackPrinter:Stack->seq<'c>) (gridCharsRow:gridCharsRow<seq<'c>>) eol (stacks:Stack list) =
    seq {
        yield! gridCharsRow.l
        yield! simpleInterleave stackPrinter gridCharsRow.m stacks
        yield! gridCharsRow.r
        yield! eol
    }

// Print a band
let printBand (rowToSeq:Row->seq<'c>) (rowSeparator:seq<'c>) (bandRows:Band->Row list) (band:Band) =
    simpleInterleave rowToSeq rowSeparator (bandRows band)

// Print a puzzle grid, supply callback to draw each cell
let printGrid (gridChars:gridChars<seq<'c>>) (eol:seq<'c>) (symbolTo:Cell->'c) (puzzleMaps:PuzzleMaps) =

    let doPrintColumn  = printColumn (printCell symbolTo)

    let doPrintStack = printStack doPrintColumn Seq.empty puzzleMaps.stackColumns

    let doPrintRow row = printRow (doPrintStack row) gridChars.v eol puzzleMaps.stacks

    let doPrintBand = printBand doPrintRow Seq.empty puzzleMaps.bandRows

    let r = Seq.collect (konst gridChars.h) (puzzleMaps.stackColumns puzzleMaps.stacks.Head)

    let printHorizontal (g:gridCharsRow<seq<'c>>) =
        sinterleave (konst r) g.l g.m g.r eol puzzleMaps.stacks

    let t = printHorizontal gridChars.t
    let m = printHorizontal gridChars.m
    let b = printHorizontal gridChars.b

    sinterleave doPrintBand t m b Seq.empty puzzleMaps.bands

let print_full (solutionChars:solutionChars<seq<'c>>) (eol:seq<'c>) (symbolTo:Cell->Candidate->'b) (puzzleMaps:PuzzleMaps) (alphabet:Candidate list) (draw_cell:Candidate->'b ->'c) =

    let d = Seq.collect (konst solutionChars.h) (puzzleMaps.stackColumns puzzleMaps.stacks.Head)
    let i = Seq.collect (konst solutionChars.hi) (puzzleMaps.stackColumns puzzleMaps.stacks.Head)

    let printFullHorizontal (x:solutionCharsRow<seq<'c>>) i =
        let s = simpleInterleave (konst i) x.mi (puzzleMaps.stackColumns puzzleMaps.stacks.Head)

        sinterleave (konst s) x.x.l x.x.m x.x.r eol puzzleMaps.stacks


    let c = List.length (puzzleMaps.stackColumns puzzleMaps.stacks.Head)
    let s = List.toSeq alphabet
    let ss = seq { for i in 0 .. puzzleMaps.stacks.Length - 1 do yield Seq.skip (i * c) s |> Seq.take c }
    let sss = List.ofSeq ss
    let ssss = List.map List.ofSeq sss

    let doPrintColumn symbols =
        let doPrintCell cell = Seq.map (fun symbol -> symbolTo cell symbol |> draw_cell symbol) symbols
        printColumn doPrintCell

    let doPrintStack symbols = printStack (doPrintColumn symbols) solutionChars.vi puzzleMaps.stackColumns

    let doPrintRow row =
        Seq.collect (
            fun symbols ->
                printRow (doPrintStack symbols row) solutionChars.v eol puzzleMaps.stacks)
            ssss

    let t = printFullHorizontal solutionChars.t d
    let m = printFullHorizontal solutionChars.m d
    let b = printFullHorizontal solutionChars.b d

    let rowSeparator = printFullHorizontal solutionChars.mi i

    let doPrintBand = printBand doPrintRow rowSeparator puzzleMaps.bandRows


    sinterleave doPrintBand t m b Seq.empty puzzleMaps.bands
