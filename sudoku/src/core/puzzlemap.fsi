module core.Puzzlemap

open Sudoku
open oset
open smap

[<NoComparison; NoEquality>]
type puzzleMap =
    {
        columns : OSet<column>;
        rows : OSet<row>;
        cells : OSet<cell>;
        stacks : OSet<stack>;
        bands : OSet<band>;
        boxes : OSet<bbox>;
        houses : OSet<house>;
        (* for a column, return the cells in it *)
        columnCells : SMap<column, OSet<cell>>;
        (* for a row, return the cells in it *)
        rowCells : SMap<row, OSet<cell>>;
        (* for a column, which stack is it in? *)
        columnStack : SMap<column, stack>;
        (* for a stack, return the columns in it *)
        stackColumns : SMap<stack, OSet<column>>;
        (* for a row, which band is it in? *)
        rowBand : SMap<row, band>;
        (* for a band, return the rows in it *)
        bandRows : SMap<band, OSet<row>>;
        (* for a cell, which box is it in? *)
        cellBox : SMap<cell, bbox>;
        (* for a box, return the cells in it *)
        boxCells : SMap<bbox, OSet<cell>>;
        (* for a house, return the cells in it *)
        houseCells : SMap<house, OSet<cell>>;
        cellHouseCells : SMap<cell, OSet<cell>>;
        housesCells : OSet<house> -> OSet<cell>;
        houseCellCandidateReductions : house -> cellCandidates -> candidateReduction list;

        (*abstract member houseCellCandidates : (house, cellCandidates) list*)
    }

val tPuzzleMap : puzzleShape -> puzzleMap
