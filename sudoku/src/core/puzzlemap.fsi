module core.Puzzlemap

open Sudoku
open compat.oset
open compat.smap

[<NoComparison; NoEquality>]
type puzzleMap =
    {
        columns : columns;
        rows : rows;
        cells : cells;
        stacks : stacks;
        bands : bands;
        boxes : boxes;
        houses : houses;
        (* for a column, return the cells in it *)
        columnCells : SMap<column, cells>;
        (* for a row, return the cells in it *)
        rowCells : SMap<row, cells>;
        (* for a column, which stack is it in? *)
        columnStack : SMap<column, stack>;
        (* for a stack, return the columns in it *)
        stackColumns : SMap<stack, columns>;
        (* for a row, which band is it in? *)
        rowBand : SMap<row, band>;
        (* for a band, return the rows in it *)
        bandRows : SMap<band, rows>;
        (* for a cell, which box is it in? *)
        cellBox : SMap<cell, bbox>;
        (* for a box, return the cells in it *)
        boxCells : SMap<bbox, cells>;
        (* for a house, return the cells in it *)
        houseCells : SMap<house, cells>;
        cellHouseCells : SMap<cell, cells>;
        housesCells : houses -> cells;
        houseCellCandidateReductions : house -> cellCandidates -> candidateReduction list;

        (*abstract member houseCellCandidates : (house, cellCandidates) list*)
    }

val tPuzzleMap : puzzleShape -> puzzleMap
