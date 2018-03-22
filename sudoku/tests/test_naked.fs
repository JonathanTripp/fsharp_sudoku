module tests.Test_naked

open core.Sudoku

open NUnit.Framework
open oset

[<Test>]
let ``Can find naked singles``() =
    let sudoku = "000105000140000670080002400063070010900000003010090520007200080026000035000409000" in

    let p = core.Puzzlemap.tPuzzleMap PuzzleShape.default' in

    let solution = input.Load.load PuzzleShape.default' sudoku in

    let candidateReductions = core.LoadEliminate.find p solution.current in
    let newSolution = core.LoadEliminate.step p solution candidateReductions in

    let cellCandidates = Solution.currentCellCandidates p.cells newSolution.current in

    let hints = hints.Naked.find 1 p cellCandidates in

    let expectedHints : core.Hint.description list =
        [   { primaryHouses = Houses.empty;
              secondaryHouses = Houses.empty;
              candidateReductions = OSet.empty;
              setCellValueAction = Some (Value.make (Cell.make (Column.make 8) (Row.make 1)) (Digits.nth PuzzleShape.default'.alphabet 8));
              pointers = OSet.empty;
              focus = Digits.empty };
            { primaryHouses = Houses.empty;
              secondaryHouses = Houses.empty;
              candidateReductions = OSet.empty;
              setCellValueAction = Some (Value.make (Cell.make (Column.make 8) (Row.make 9)) (Digits.nth PuzzleShape.default'.alphabet 5));
              pointers = OSet.empty;
              focus = Digits.empty } ]
        in

    let _ = Assert.AreEqual(2, List.length hints) in
    Assert.AreEqual(expectedHints, hints)
