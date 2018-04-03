module tests.Test_core

open core.Sudoku

open NUnit.Framework
open oset
open smap

let twoByFourPuzzleSpec =
    { size = 8;
      boxWidth = 2;
      boxHeight = 4;
      alphabet = 
            [1..8]
            |> OSet.ofList
            |> OSet.map (fun i -> (char) i + '0' |> Digit)
             }

let pick_some (as' : List<'a>) : List<'a> * List<'a> =
    let picked =
        [9; 5; 2; 5; 5; 5; 1; 8; 9; 3; 5; 6]
        |> List.map (fun i -> List.item (i - 1) as')
        in

    let expected =
        [1; 2; 3; 5; 6; 8; 9]
        |> List.map (fun i -> List.item (i - 1) as')
        in

    (picked, expected)

let pick_more<'a when 'a : comparison> (as' : List<'a>) : List<'a> * List<'a> =
    let picked =
        [9 * 8 + 1; 9 * 0 + 5; 9 * 2 + 4; 9 * 0 + 5; 9 * 0 + 5; 9 * 5 + 1; 9 * 0 + 8; 9 * 8 + 1; 9 * 3 + 3; 9 * 0 + 6]
        |> List.map (fun i -> List.item (i - 1) as')
        in

    let expected =
        [9 * 0 + 5; 9 * 0 + 6; 9 * 0 + 8; 9 * 2 + 4; 9 * 3 + 3; 9 * 5 + 1; 9 * 8 + 1]
        |> List.map (fun i -> List.item (i - 1) as')
        in

    (picked, expected)

[<Test>]
let ``Can make column sets``() =
    let p = core.Puzzlemap.tPuzzleMap PuzzleShape.default' in

    let (picked, expected) = pick_some p.columns in

    Assert.AreEqual(expected, picked, "{0}!={1}", Columns.toString expected, Columns.toString picked)

[<Test>]
let ``Can make row sets``() =
    let p = core.Puzzlemap.tPuzzleMap PuzzleShape.default' in

    let (picked, expected) = pick_some p.rows in

    Assert.AreEqual(expected, picked, "{0}!={1}", Rows.toString expected, Rows.toString picked)

[<Test>]
let ``Can make cell sets``() =
    let p = core.Puzzlemap.tPuzzleMap PuzzleShape.default' in

    let (picked, expected) = pick_more p.cells in

    Assert.AreEqual(expected, picked, "{0}!={1}", Cells.toString expected, Cells.toString picked)

[<Test>]
let ``Can make digit sets``() =
    let p = core.Puzzlemap.tPuzzleMap PuzzleShape.default' in

    let (picked, expected) = pick_some PuzzleShape.default'.alphabet in

    Assert.AreEqual(expected, picked, "{0}!={1}", Digits.toString expected, Digits.toString picked)

[<Test>]
let ``Can make columns``() =
    let p = core.Puzzlemap.tPuzzleMap PuzzleShape.default' in
    let actual = p.columns in

    let expected =
        [1..9]
        |> List.map CColumn
        |> OSet.ofList
        in

    Assert.AreEqual(expected, actual, "{0}!={1}", Columns.toString expected, Columns.toString actual)

[<Test>]
let ``Can print columns``() =
    let p = core.Puzzlemap.tPuzzleMap PuzzleShape.default' in
    let actual = p.columns in

    let first = OSet.item 0 actual
    let printedFirst = first.ToString()

    Assert.AreEqual("c1", printedFirst)

    let fourth = OSet.item 3 actual
    let printedFourth = fourth.ToString()

    Assert.AreEqual("c4", printedFourth)

    let last = OSet.item 8 actual
    let printedLast = last.ToString()

    Assert.AreEqual("c9", printedLast)

[<Test>]
let ``Can print columns2``() =
    let p = core.Puzzlemap.tPuzzleMap PuzzleShape.default' in
    let actual = p.columns in
    let middleThree = actual |> OSet.skip 3 |> OSet.take 3
    let printedMiddleThree = middleThree.ToString()

    Assert.AreEqual("{c4,c5,c6}", printedMiddleThree)

[<Test>]
let ``Can make rows``() = 
    let p = core.Puzzlemap.tPuzzleMap PuzzleShape.default' in
    let actual = p.rows in

    let expected =
        [1..9]
        |> List.map RRow
        |> OSet.ofList
        in

    Assert.AreEqual(expected, actual, "{0}!={1}", Rows.toString expected, Rows.toString actual)

[<Test>]
let ``Can print rows``() =
    let p = core.Puzzlemap.tPuzzleMap PuzzleShape.default' in
    let actual = p.rows in

    let first = OSet.item 0 actual
    let printedFirst = first.ToString()

    Assert.AreEqual("r1", printedFirst)

    let fourth = OSet.item 3 actual
    let printedFourth = fourth.ToString()

    Assert.AreEqual("r4", printedFourth)

    let last = OSet.item 8 actual
    let printedLast = last.ToString()

    Assert.AreEqual("r9", printedLast)

[<Test>]
let ``Can print rows2``() =
    let p = core.Puzzlemap.tPuzzleMap PuzzleShape.default' in
    let actual = p.rows in
    let middleThree = actual |> OSet.skip 3 |> OSet.take 3
    let printedMiddleThree = middleThree.ToString()

    Assert.AreEqual("{r4,r5,r6}", printedMiddleThree)

[<Test>]
let ``Can make cells``() = 
    let p = core.Puzzlemap.tPuzzleMap PuzzleShape.default' in
    let actual = p.cells in

    let expected =
        [1..9]
        |> List.map
            (fun r ->
                [1..9]
                |> List.map
                    (fun c -> Cell.make (c |> CColumn) (r |> RRow)))
        |> List.concat
        |> OSet.ofList
        in

    Assert.AreEqual(expected, actual, "{0}!={1}", Cells.toString expected, Cells.toString actual)

[<Test>]
let ``Can print cells``() = 
    let cell = Cell.make (CColumn 3) (RRow 7)
    let printedCell = cell.ToString()

    Assert.AreEqual("c3r7", printedCell)

[<Test>]
let ``Can make stacks``() = 
    let p = core.Puzzlemap.tPuzzleMap twoByFourPuzzleSpec in
    let actual = p.stacks in

    let expected =
        [1..4]
        |> OSet.ofList
        |> OSet.map SStack
        in

    Assert.AreEqual(expected, actual, "{0}!={1}", Stacks.to_string expected, Stacks.to_string actual)

[<Test>]
let ``Can make bands``() = 
    let p = core.Puzzlemap.tPuzzleMap twoByFourPuzzleSpec in
    let actual = p.bands in

    let expected =
        [1..2]
        |> OSet.ofList
        |> OSet.map BBand
        in

    Assert.AreEqual(expected, actual, "{0}!={1}", Bands.to_string expected, Bands.to_string actual)

[<Test>]
let ``Can make boxes``() = 
    let p = core.Puzzlemap.tPuzzleMap twoByFourPuzzleSpec in
    let actual = p.boxes in

    let expected =
        [1..2]
        |> OSet.ofList
        |> OSet.map
            (fun b ->
                [1..4]
                |> OSet.ofList
                |> OSet.map
                    (fun s -> Box.make (s |> SStack) (b |> BBand)))
        |> OSet.concat
        in

    Assert.AreEqual(expected, actual, "{0}!={1}", Boxes.to_string expected, Boxes.to_string actual)

[<Test>]
let ``Can make houses``() = 
    let p = core.Puzzlemap.tPuzzleMap twoByFourPuzzleSpec in
    let actual = p.houses in

    let expectedColumns =
        [1..8]
        |> List.map CColumn
        |> List.map HColumn
        in

    let expectedRows =
        [1..8]
        |> List.map RRow
        |> List.map HRow
        in

    let expectedBoxes =
        [1..2]
        |> List.map
            (fun b ->
                [1..4]
                |> List.map
                    (fun s -> Box.make (s |> SStack) (b |> BBand)))
        |> List.concat
        |> List.map HBox
        in

    let expected =
        [ expectedColumns; expectedRows; expectedBoxes]
        |> List.concat
        |> OSet.ofList
        in

    Assert.AreEqual(expected, actual, "{0}!={1}", Houses.toString expected, Houses.toString actual)

[<Test>]
let ``Get column cells``() = 
    let p = core.Puzzlemap.tPuzzleMap PuzzleShape.default' in

    let column = CColumn 2 in

    let actual =
        SMap.get column p.columnCells in

    let expected =
        [1..9]
        |> List.map
            (fun r -> Cell.make column (r |> RRow))
        |> OSet.ofList
        in

    Assert.AreEqual(expected, actual, "{0}!={1}", Cells.toString expected, Cells.toString actual)

[<Test>]
let ``Get row cells``() = 
    let p = core.Puzzlemap.tPuzzleMap PuzzleShape.default' in

    let row = RRow 7 in

    let actual =
        SMap.get row p.rowCells in

    let expected =
        [1..9]
        |> List.map
            (fun c -> Cell.make (c |> CColumn) (7 |> RRow))
        |> OSet.ofList
        in

    Assert.AreEqual(expected, actual, "{0}!={1}", Cells.toString expected, Cells.toString actual)

[<Test>]
let ``Get stack for a column``() =
    let p = core.Puzzlemap.tPuzzleMap PuzzleShape.default' in

    let actual =
        p.columns
        |> OSet.map (fun column -> SMap.get column p.columnStack)
        in

    let expected =
        [1..3]
        |> OSet.ofList
        |> OSet.map
            (fun s ->
                [1..3]
                |> OSet.ofList
                |> OSet.map (fun _ -> s |> SStack))
        |> OSet.concat
        in

    Assert.AreEqual(expected, actual, "{0}!={1}", Stacks.to_string expected, Stacks.to_string actual)

[<Test>]
let ``Get stack columns``() = 
    let p = core.Puzzlemap.tPuzzleMap PuzzleShape.default' in

    let actual =
        SMap.get (2 |> SStack) p.stackColumns
        in

    let expected =
        [4..6]
        |> OSet.ofList
        |> OSet.map CColumn
        in

    Assert.AreEqual(expected, actual, "{0}!={1}", Columns.toString expected, Columns.toString actual)

[<Test>]
let ``Get band for a row``() =
    let p = core.Puzzlemap.tPuzzleMap PuzzleShape.default' in

    let actual =
        p.rows
        |> OSet.map (fun row -> SMap.get row p.rowBand)
        in

    let expected =
        [1..3]
        |> OSet.ofList
        |> OSet.map
            (fun b ->
                [1..3]
                |> OSet.ofList
                |> OSet.map (fun _ -> b |> BBand))
        |> OSet.concat
        in

    Assert.AreEqual(expected, actual, "{0}!={1}", Bands.to_string expected, Bands.to_string actual)

[<Test>]
let ``Get band rows``() = 
    let p = core.Puzzlemap.tPuzzleMap PuzzleShape.default' in

    let actual =
        SMap.get (2 |> BBand) p.bandRows
        in

    let expected =
        [4..6]
        |> OSet.ofList
        |> OSet.map RRow
        in

    Assert.AreEqual(expected, actual, "{0}!={1}", Rows.toString expected, Rows.toString actual)

[<Test>]
let ``Get box for a cell``() =
    let p = core.Puzzlemap.tPuzzleMap PuzzleShape.default' in

    let actual =
        [1..9]
        |> OSet.ofList
        |> OSet.map
            (fun r -> Cell.make (5 |> CColumn) (r |> RRow))
        |> OSet.map (fun cell -> SMap.get cell p.cellBox)
        in

    let expected =
        [1..3]
        |> OSet.ofList
        |> OSet.map
            (fun b ->
                [1..3]
                |> OSet.ofList
                |> OSet.map
                    (fun _ -> Box.make (2 |> SStack) (b |> BBand)))
        |> OSet.concat
        in

    Assert.AreEqual(expected, actual, "{0}!={1}", Boxes.to_string expected, Boxes.to_string actual)

let all_tests =
    [
    ``Can make column sets``;
    ``Can make row sets``;
    ``Can make cell sets``;
    ``Can make digit sets``;
    ``Can make columns``;
    ``Can make rows``;
    ``Can make cells``;
    ``Can make stacks``;
    ``Can make bands``;
    ``Can make boxes``;
    ``Can make houses``;
    ``Get column cells``;
    ``Get row cells``;
    ``Get stack for a column``;
    ``Get stack columns``;
    ``Get band for a row``;
    ``Get band rows``;
    ``Get box for a cell``;
    ]
