module Sudoku.Tests.Test_core

open Microsoft.VisualStudio.TestTools.UnitTesting

open Sudoku.Lib.compat.oset
open Sudoku.Lib.compat.smap

open Sudoku.Lib.core
open Sudoku.Lib.core.Sudoku

let twoByFourPuzzleSpec : puzzleShape =
    { size = 8;
      boxWidth = 2;
      boxHeight = 4;
      alphabet = 
            [1..8]
            |> List.map (fun i -> (char) i + '0' |> Digit)
            |> OSet.ofList
             }

let pick_some (as' : OSet<'a>) : OSet<'a> * OSet<'a> =
    let as'' = OSet.toList as'

    let actual : OSet<'a> =
        [9; 5; 2; 5; 5; 5; 1; 8; 9; 3; 5; 6]
        |> List.map (fun i -> List.item (i - 1) as'')
        |> OSet.ofList
        in

    let expected : OSet<'a> =
        [1; 2; 3; 5; 6; 8; 9]
        |> List.map (fun i -> List.item (i - 1) as'')
        |> OSet.ofList
        in

    (actual, expected)

let pick_more (as' : OSet<'a>) : OSet<'a> * OSet<'a> =
    let as'' = OSet.toList as'

    let actual : OSet<'a> =
        [9 * 8 + 1; 9 * 0 + 5; 9 * 2 + 4; 9 * 0 + 5; 9 * 0 + 5; 9 * 5 + 1; 9 * 0 + 8; 9 * 8 + 1; 9 * 3 + 3; 9 * 0 + 6]
        |> List.map (fun i -> List.item (i - 1) as'')
        |> OSet.ofList
        in

    let expected : OSet<'a> =
        [9 * 0 + 5; 9 * 0 + 6; 9 * 0 + 8; 9 * 2 + 4; 9 * 3 + 3; 9 * 5 + 1; 9 * 8 + 1]
        |> List.map (fun i -> List.item (i - 1) as'')
        |> OSet.ofList
        in

    (actual, expected)

[<TestClass>]
type TestCore() =

    [<TestMethod>]
    member this.``Can make column sets``() =
        let p = Puzzlemap.tPuzzleMap PuzzleShape.default' in

        let (actual, expected) = pick_some p.columns in

        let eq = OSet.equals expected actual in
        Assert.AreEqual(true, eq, "{0}!={1}")

    [<TestMethod>]
    member this.``Can make row sets``() =
        let p = Puzzlemap.tPuzzleMap PuzzleShape.default' in

        let (actual, expected) = pick_some p.rows in

        let eq = OSet.equals expected actual in
        Assert.AreEqual(true, eq, "{0}!={1}")

    [<TestMethod>]
    member this.``Can make cell sets``() =
        let p = Puzzlemap.tPuzzleMap PuzzleShape.default' in

        let (actual, expected) = pick_more p.cells in

        let eq = OSet.equals expected actual in
        Assert.AreEqual(true, eq, "{0}!={1}")

    [<TestMethod>]
    member this.``Can make digit sets``() =
        let p = Puzzlemap.tPuzzleMap PuzzleShape.default' in

        let (actual, expected) = pick_some PuzzleShape.default'.alphabet in

        let eq = OSet.equals expected actual in
        Assert.AreEqual(true, eq, "{0}!={1}")

    [<TestMethod>]
    member this.``Can make columns``() =
        let p = Puzzlemap.tPuzzleMap PuzzleShape.default' in
        let actual = p.columns in

        let expected : columns =
            [1..9]
            |> List.map CColumn
            |> OSet.ofList
            in

        let eq = OSet.equals expected actual in
        Assert.AreEqual(true, eq, "{0}!={1}", Columns.print expected, Columns.print actual)

    [<TestMethod>]
    member this.``Can print columns``() =
        let p = Puzzlemap.tPuzzleMap PuzzleShape.default' in
        let actual = p.columns in

        let first = OSet.item 0 actual
        let printedFirst = Column.print first

        Assert.AreEqual("c1", printedFirst)

        let fourth = OSet.item 3 actual
        let printedFourth = Column.print fourth

        Assert.AreEqual("c4", printedFourth)

        let last = OSet.item 8 actual
        let printedLast = Column.print last

        Assert.AreEqual("c9", printedLast)

    [<TestMethod>]
    member this.``Can print columns2``() =
        let p = Puzzlemap.tPuzzleMap PuzzleShape.default' in
        let actual = p.columns in
        let middleThree = actual |> OSet.skip 3 |> OSet.take 3
        let printedMiddleThree = Columns.print middleThree

        Assert.AreEqual("{c4,c5,c6}", printedMiddleThree)

    [<TestMethod>]
    member this.``Can make rows``() = 
        let p = Puzzlemap.tPuzzleMap PuzzleShape.default' in
        let actual = p.rows in

        let expected : rows =
            [1..9]
            |> List.map RRow
            |> OSet.ofList
            in

        let eq = OSet.equals expected actual in
        Assert.AreEqual(true, eq, "{0}!={1}", Rows.print expected, Rows.print actual)

    [<TestMethod>]
    member this.``Can print rows``() =
        let p = Puzzlemap.tPuzzleMap PuzzleShape.default' in
        let actual = p.rows in

        let first = OSet.item 0 actual
        let printedFirst = Row.print first

        Assert.AreEqual("r1", printedFirst)

        let fourth = OSet.item 3 actual
        let printedFourth = Row.print fourth

        Assert.AreEqual("r4", printedFourth)

        let last = OSet.item 8 actual
        let printedLast = Row.print last

        Assert.AreEqual("r9", printedLast)

    [<TestMethod>]
    member this.``Can print rows2``() =
        let p = Puzzlemap.tPuzzleMap PuzzleShape.default' in
        let actual = p.rows in
        let middleThree = actual |> OSet.skip 3 |> OSet.take 3
        let printedMiddleThree = Rows.print middleThree

        Assert.AreEqual("{r4,r5,r6}", printedMiddleThree)

    [<TestMethod>]
    member this.``Can make cells``() = 
        let p = Puzzlemap.tPuzzleMap PuzzleShape.default' in
        let actual = p.cells in

        let expected : cells =
            [1..9]
            |> List.map
                (fun r ->
                    [1..9]
                    |> List.map
                        (fun c -> Cell.make (c |> CColumn) (r |> RRow)))
            |> List.concat
            |> OSet.ofList
            in

        let eq = OSet.equals expected actual in
        Assert.AreEqual(true, eq, "{0}!={1}", Cells.print expected, Cells.print actual)

    [<TestMethod>]
    member this.``Can print cells``() = 
        let cell = Cell.make (CColumn 3) (RRow 7)
        let printedCell = Cell.print cell

        Assert.AreEqual("c3r7", printedCell)

    [<TestMethod>]
    member this.``Can make stacks``() = 
        let p = Puzzlemap.tPuzzleMap twoByFourPuzzleSpec in
        let actual = p.stacks in

        let expected : stacks =
            [1..4]
            |> List.map SStack
            |> OSet.ofList
            in

        let eq = OSet.equals expected actual in
        Assert.AreEqual(true, eq, "{0}!={1}", Stacks.print expected, Stacks.print actual)

    [<TestMethod>]
    member this.``Can make bands``() = 
        let p = Puzzlemap.tPuzzleMap twoByFourPuzzleSpec in
        let actual = p.bands in

        let expected : bands =
            [1..2]
            |> List.map BBand
            |> OSet.ofList
            in

        let eq = OSet.equals expected actual in
        Assert.AreEqual(true, eq, "{0}!={1}", Bands.print expected, Bands.print actual)

    [<TestMethod>]
    member this.``Can make boxes``() = 
        let p = Puzzlemap.tPuzzleMap twoByFourPuzzleSpec in
        let actual = p.boxes in

        let expected : boxes =
            [1..2]
            |> List.map
                (fun b ->
                    [1..4]
                    |> List.map
                        (fun s -> Box.make (s |> SStack) (b |> BBand))
                    |> OSet.ofList)
            |> OSet.concat
            in

        let eq = OSet.equals expected actual in
        Assert.AreEqual(true, eq, "{0}!={1}", Boxes.print expected, Boxes.print actual)

    [<TestMethod>]
    member this.``Can make houses``() = 
        let p = Puzzlemap.tPuzzleMap twoByFourPuzzleSpec in
        let actual = p.houses in

        let expectedColumns : house list=
            [1..8]
            |> List.map CColumn
            |> List.map HColumn
            in

        let expectedRows : house list =
            [1..8]
            |> List.map RRow
            |> List.map HRow
            in

        let expectedBoxes : house list =
            [1..2]
            |> List.map
                (fun b ->
                    [1..4]
                    |> List.map
                        (fun s -> Box.make (s |> SStack) (b |> BBand)))
            |> List.concat
            |> List.map HBox
            in

        let expected : houses =
            [ expectedColumns; expectedRows; expectedBoxes]
            |> List.concat
            |> OSet.ofList
            in

        let eq = OSet.equals expected actual in
        Assert.AreEqual(true, eq, "{0}!={1}", Houses.print expected, Houses.print actual)

    [<TestMethod>]
    member this.``Get column cells``() = 
        let p = Puzzlemap.tPuzzleMap PuzzleShape.default' in

        let column = CColumn 2 in

        let actual : cells =
            SMap.get column p.columnCells in

        let expected : cells =
            [1..9]
            |> List.map
                (fun r -> Cell.make column (r |> RRow))
            |> OSet.ofList
            in

        let eq = OSet.equals expected actual in
        Assert.AreEqual(true, eq, "{0}!={1}", Cells.print expected, Cells.print actual)

    [<TestMethod>]
    member this.``Get row cells``() = 
        let p = Puzzlemap.tPuzzleMap PuzzleShape.default' in

        let row = RRow 7 in

        let actual : cells =
            SMap.get row p.rowCells in

        let expected : cells =
            [1..9]
            |> List.map
                (fun c -> Cell.make (c |> CColumn) (7 |> RRow))
            |> OSet.ofList
            in

        let eq = OSet.equals expected actual in
        Assert.AreEqual(true, eq, "{0}!={1}", Cells.print expected, Cells.print actual)

    [<TestMethod>]
    member this.``Get stack for a column``() =
        let p = Puzzlemap.tPuzzleMap PuzzleShape.default' in

        let actual : stacks =
            p.columns
            |> OSet.map (fun column -> SMap.get column p.columnStack)
            in

        let expected : stacks =
            [1..3]
            |> List.map
                (fun s ->
                    [1..3]
                    |> List.map (fun _ -> s |> SStack)
                    |> OSet.ofList)
            |> OSet.concat
            in

        let eq = OSet.equals expected actual in
        Assert.AreEqual(true, eq, "{0}!={1}", Stacks.print expected, Stacks.print actual)

    [<TestMethod>]
    member this.``Get stack columns``() = 
        let p = Puzzlemap.tPuzzleMap PuzzleShape.default' in

        let actual : columns =
            SMap.get (2 |> SStack) p.stackColumns
            in

        let expected : columns =
            [4..6]
            |> List.map CColumn
            |> OSet.ofList
            in

        let eq = OSet.equals expected actual in
        Assert.AreEqual(true, eq, "{0}!={1}", Columns.print expected, Columns.print actual)

    [<TestMethod>]
    member this.``Get band for a row``() =
        let p = Puzzlemap.tPuzzleMap PuzzleShape.default' in

        let actual : bands =
            p.rows
            |> OSet.map (fun row -> SMap.get row p.rowBand)
            in

        let expected : bands =
            [1..3]
            |> List.map
                (fun b ->
                    [1..3]
                    |> List.map (fun _ -> b |> BBand)
                    |> OSet.ofList)
            |> OSet.concat
            in

        let eq = OSet.equals expected actual in
        Assert.AreEqual(true, eq, "{0}!={1}", Bands.print expected, Bands.print actual)

    [<TestMethod>]
    member this.``Get band rows``() = 
        let p = Puzzlemap.tPuzzleMap PuzzleShape.default' in

        let actual : rows =
            SMap.get (2 |> BBand) p.bandRows
            in

        let expected : rows =
            [4..6]
            |> List.map RRow
            |> OSet.ofList
            in

        let eq = OSet.equals expected actual in
        Assert.AreEqual(true, eq, "{0}!={1}", Rows.print expected, Rows.print actual)

    [<TestMethod>]
    member this.``Get box for a cell``() =
        let p = Puzzlemap.tPuzzleMap PuzzleShape.default' in

        let actual : boxes =
            [1..9]
            |> List.map
                (fun r -> Cell.make (5 |> CColumn) (r |> RRow))
            |> OSet.ofList
            |> OSet.map (fun cell -> SMap.get cell p.cellBox)
            in

        let expected : boxes =
            [1..3]
            |> List.map
                (fun b ->
                    [1..3]
                    |> List.map
                        (fun _ -> Box.make (2 |> SStack) (b |> BBand))
                    |> OSet.ofList)
            |> OSet.concat
            in

        let eq = OSet.equals expected actual in
        Assert.AreEqual(true, eq, "{0}!={1}", Boxes.print expected, Boxes.print actual)

let instance = new TestCore()

let all_tests =
    [
    instance.``Can make column sets``;
    instance.``Can make row sets``;
    instance.``Can make cell sets``;
    instance.``Can make digit sets``;
    instance.``Can make columns``;
    instance.``Can make rows``;
    instance.``Can make cells``;
    instance.``Can make stacks``;
    instance.``Can make bands``;
    instance.``Can make boxes``;
    instance.``Can make houses``;
    instance.``Get column cells``;
    instance.``Get row cells``;
    instance.``Get stack for a column``;
    instance.``Get stack columns``;
    instance.``Get band for a row``;
    instance.``Get band rows``;
    instance.``Get box for a cell``;
    ]
