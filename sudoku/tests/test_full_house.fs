module test.Test_full_house

open core.Sudoku

open NUnit.Framework
open oset

[<Test>]
let ``Can find full house``() =
    let sudoku = "800739006370465000040182009000600040054300610060500000400853070000271064100940002" in

    let p = core.Puzzlemap.tPuzzleMap PuzzleShape.default' in

    let solution = input.Load.load PuzzleShape.default' sudoku in

    let candidateReductions = core.LoadEliminate.find p solution.current in
    let newSolution = core.LoadEliminate.step p solution candidateReductions in

    let cellCandidates = Solution.currentCellCandidates p.cells newSolution.current in

    let hints = hints.FullHouse.find p cellCandidates in

    let expectedHints : core.Hint.description list =
        [   { primaryHouses = OSet.singleton (HBox (Box.make (Stack.make 2) (Band.make 3)));
              secondaryHouses = OSet.empty;
              candidateReductions = OSet.empty;
              setCellValueAction = Some (Value.make (Cell.make (Column.make 6) (Row.make 9)) (OSet.item 5 PuzzleShape.default'.alphabet));
              pointers = OSet.empty;
              focus = OSet.empty } ]
        in

    let _ = Assert.AreEqual(1, OSet.count hints) in
    Assert.AreEqual(expectedHints, hints)