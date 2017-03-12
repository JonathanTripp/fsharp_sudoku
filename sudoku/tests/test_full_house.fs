module test.Test_full_house

open core.Sudoku

open NUnit.Framework

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
        [   { primaryHouses = Houses.singleton (HBox (Box.make (Stack.make 2) (Band.make 3)));
              secondaryHouses = Houses.empty;
              candidateReductions = [];
              setCellValueAction = Some (Value.make (Cell.make (Column.make 6) (Row.make 9)) (Digits.nth PuzzleShape.default'.alphabet 5));
              pointers = [];
              focus = Digits.empty } ]
        in

    let _ = Assert.AreEqual(1, List.length hints) in
    Assert.AreEqual(expectedHints, hints)
