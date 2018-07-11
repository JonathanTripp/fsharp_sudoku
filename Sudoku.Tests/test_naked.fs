module Sudoku.Tests.Test_naked

open Microsoft.VisualStudio.TestTools.UnitTesting

open Sudoku.Lib.compat.oset

open Sudoku.Lib
open Sudoku.Lib.core
open Sudoku.Lib.core.Sudoku
open Sudoku.Lib.core.Hint

[<TestClass>]
type TestFullHouse() =

    [<TestMethod>]
    member this.``Can find naked singles``() =
        let sudoku = "000105000140000670080002400063070010900000003010090520007200080026000035000409000" in

        let p = core.Puzzlemap.tPuzzleMap PuzzleShape.default' in

        let solution = Load.load PuzzleShape.default' sudoku in

        let candidateReductions = core.LoadEliminate.find p solution.current in
        let newSolution = core.LoadEliminate.step p solution candidateReductions in

        let cellCandidates = Solution.currentCellCandidates p.cells newSolution.current in

        let hints = hints.Naked.find 1 p cellCandidates in

        let expectedHints : descriptions =
            [   { primaryHouses = OSet.empty();
                  secondaryHouses = OSet.empty();
                  candidateReductions = [];
                  setCellValueAction = Some (Value.make (Cell.make (Column.ofNat 8) (Row.ofNat 1)) (OSet.item 8 PuzzleShape.default'.alphabet));
                  pointers = [];
                  focus = OSet.empty() };
                { primaryHouses = OSet.empty();
                  secondaryHouses = OSet.empty();
                  candidateReductions = [];
                  setCellValueAction = Some (Value.make (Cell.make (Column.ofNat 8) (Row.ofNat 9)) (OSet.item 5 PuzzleShape.default'.alphabet));
                  pointers = [];
                  focus = OSet.empty() } ]
            in

        let _ = Assert.AreEqual(2, List.length hints) in
        Assert.AreEqual(expectedHints, hints)
