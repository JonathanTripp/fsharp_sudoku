module tests.Test_core

open core.Sudoku

open NUnit.Framework
open oset

let twoByFourPuzzleSpec =
    { size = 8;
      boxWidth = 2;
      boxHeight = 4;
      alphabet = 
            [1..8]
            |> List.map (fun i -> (char) i + '0' |> Digit)
            |> Digits.make }

let pick_some (as' : 'a list) : 'a list * 'a list =
    let picked =
        [9; 5; 2; 5; 5; 5; 1; 8; 9; 3; 5; 6]
        |> List.map (fun i -> List.nth as' (i - 1))
        in

    let expected =
        [1; 2; 3; 5; 6; 8; 9]
        |> List.map (fun i -> List.nth as' (i - 1))
        in

    (picked, expected)

let pick_more<'a> (as' : 'a list) : 'a list * 'a list =
    let picked =
        [9 * 8 + 1; 9 * 0 + 5; 9 * 2 + 4; 9 * 0 + 5; 9 * 0 + 5; 9 * 5 + 1; 9 * 0 + 8; 9 * 8 + 1; 9 * 3 + 3; 9 * 0 + 6]
        |> List.map (fun i -> List.nth as' (i - 1))
        in

    let expected =
        [9 * 0 + 5; 9 * 0 + 6; 9 * 0 + 8; 9 * 2 + 4; 9 * 3 + 3; 9 * 5 + 1; 9 * 8 + 1]
        |> List.map (fun i -> List.nth as' (i - 1))
        in

    (picked, expected)

[<Test>]
let ``Can make column sets``() =
    let p = core.Puzzlemap.tPuzzleMap PuzzleShape.default' in

    let (picked, expected) = pick_some (OSet.toList p.columns) in

    let picked' =
        picked
        |> OSet.ofList
        in

    let expected' =
        expected
        |> OSet.ofList
        in

    Assert.AreEqual(expected, picked', "{0}!={1}", Columns.toString expected', Columns.toString picked')

[<Test>]
let ``Can make row sets``() =
    let p = core.Puzzlemap.tPuzzleMap PuzzleShape.default' in

    let (picked, expected) = pick_some (Rows.to_list p.rows) in

    let picked' =
        picked
        |> Rows.make
        in

    let expected' =
        expected
        |> Rows.make
        in

    Assert.AreEqual(expected, picked', "{0}!={1}", Rows.to_string expected', Rows.to_string picked')

[<Test>]
let ``Can make cell sets``() =
    let p = core.Puzzlemap.tPuzzleMap PuzzleShape.default' in

    let (picked, expected) = pick_more (Cells.to_list p.cells) in

    let picked' =
        picked
        |> Cells.make
        in

    let expected' =
        expected
        |> Cells.make
        in

    Assert.AreEqual(expected, picked', "{0}!={1}", Cells.to_string expected', Cells.to_string picked')

[<Test>]
let ``Can make digit sets``() =
    let p = core.Puzzlemap.tPuzzleMap PuzzleShape.default' in

    let (picked, expected) = pick_some (Digits.to_list PuzzleShape.default'.alphabet) in

    let picked' =
        picked
        |> Digits.make
        in

    let expected' =
        expected
        |> Digits.make
        in

    Assert.AreEqual(expected, picked', "{0}!={1}", Digits.to_string expected', Digits.to_string picked')

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
let ``Can make rows``() = 
    let p = core.Puzzlemap.tPuzzleMap PuzzleShape.default' in
    let actual = p.rows in

    let expected =
        [1..9]
        |> List.map RRow
        |> Rows.make
        in

    Assert.AreEqual(expected, actual, "{0}!={1}", Rows.to_string expected, Rows.to_string actual)

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
        |> Cells.make
        in

    Assert.AreEqual(expected, actual, "{0}!={1}", Cells.to_string expected, Cells.to_string actual)

[<Test>]
let ``Can make stacks``() = 
    let p = core.Puzzlemap.tPuzzleMap twoByFourPuzzleSpec in
    let actual = p.stacks in

    let expected =
        [1..4]
        |> List.map SStack
        in

    Assert.AreEqual(expected, actual, "{0}!={1}", Stacks.to_string expected, Stacks.to_string actual)

[<Test>]
let ``Can make bands``() = 
    let p = core.Puzzlemap.tPuzzleMap twoByFourPuzzleSpec in
    let actual = p.bands in

    let expected =
        [1..2]
        |> List.map BBand
        in

    Assert.AreEqual(expected, actual, "{0}!={1}", Bands.to_string expected, Bands.to_string actual)

[<Test>]
let ``Can make boxes``() = 
    let p = core.Puzzlemap.tPuzzleMap twoByFourPuzzleSpec in
    let actual = p.boxes in

    let expected =
        [1..2]
        |> List.map
            (fun b ->
                [1..4]
                |> List.map
                    (fun s -> Box.make (s |> SStack) (b |> BBand)))
        |> List.concat
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
        |> Houses.make
        in

    Assert.AreEqual(expected, actual, "{0}!={1}", Houses.to_string expected, Houses.to_string actual)

[<Test>]
let ``Get column cells``() = 
    let p = core.Puzzlemap.tPuzzleMap PuzzleShape.default' in

    let column = CColumn 2 in

    let actual =
        Smap.get Column.comparer column p.columnCells in

    let expected =
        [1..9]
        |> List.map
            (fun r -> Cell.make column (r |> RRow))
        |> Cells.make
        in

    Assert.AreEqual(expected, actual, "{0}!={1}", Cells.to_string expected, Cells.to_string actual)

[<Test>]
let ``Get row cells``() = 
    let p = core.Puzzlemap.tPuzzleMap PuzzleShape.default' in

    let row = RRow 7 in

    let actual =
        Smap.get Row.comparer row p.rowCells in

    let expected =
        [1..9]
        |> List.map
            (fun c -> Cell.make (c |> CColumn) (7 |> RRow))
        |> Cells.make
        in

    Assert.AreEqual(expected, actual, "{0}!={1}", Cells.to_string expected, Cells.to_string actual)

[<Test>]
let ``Get stack for a column``() =
    let p = core.Puzzlemap.tPuzzleMap PuzzleShape.default' in

    let actual =
        p.columns
        |> OSet.map (fun column -> Smap.get Column.comparer column p.columnStack)
        in

    let expected =
        [1..3]
        |> List.map
            (fun s ->
                [1..3]
                |> List.map (fun _ -> s |> SStack))
        |> List.concat
        in

    Assert.AreEqual(expected, actual, "{0}!={1}", Stacks.to_string expected, Stacks.to_string (actual |> OSet.toList))

[<Test>]
let ``Get stack columns``() = 
    let p = core.Puzzlemap.tPuzzleMap PuzzleShape.default' in

    let actual =
        Smap.get Stack.comparer (2 |> SStack) p.stackColumns
        |> OSet.ofList
        in

    let expected =
        [4..6]
        |> List.map CColumn
        |> OSet.ofList
        in

    Assert.AreEqual(expected, actual, "{0}!={1}", Columns.toString expected, Columns.toString actual)

[<Test>]
let ``Get band for a row``() =
    let p = core.Puzzlemap.tPuzzleMap PuzzleShape.default' in

    let actual =
        p.rows
        |> Rows.map (fun row -> Smap.get Row.comparer row p.rowBand)
        in

    let expected =
        [1..3]
        |> List.map
            (fun b ->
                [1..3]
                |> List.map (fun _ -> b |> BBand))
        |> List.concat
        in

    Assert.AreEqual(expected, actual, "{0}!={1}", Bands.to_string expected, Bands.to_string actual)

[<Test>]
let ``Get band rows``() = 
    let p = core.Puzzlemap.tPuzzleMap PuzzleShape.default' in

    let actual =
        Smap.get Band.comparer (2 |> BBand) p.bandRows
        |> Rows.make
        in

    let expected =
        [4..6]
        |> List.map RRow
        |> Rows.make
        in

    Assert.AreEqual(expected, actual, "{0}!={1}", Rows.to_string expected, Rows.to_string actual)

[<Test>]
let ``Get box for a cell``() =
    let p = core.Puzzlemap.tPuzzleMap PuzzleShape.default' in

    let actual =
        [1..9]
        |> List.map
            (fun r -> Cell.make (5 |> CColumn) (r |> RRow))
        |> List.map (fun cell -> Smap.get Cell.comparer cell p.cellBox)
        in

    let expected =
        [1..3]
        |> List.map
            (fun b ->
                [1..3]
                |> List.map
                    (fun _ -> Box.make (2 |> SStack) (b |> BBand)))
        |> List.concat
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
