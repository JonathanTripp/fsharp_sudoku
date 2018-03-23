module core.Puzzlemap

open Sudoku
open oset
open smap

let columns (length : size) : OSet<column> =
    Sset.range 1 length
    |> OSet.ofList
    |> OSet.map Column.make

let rows (length : size) : OSet<row> =
    Sset.range 1 length
    |> OSet.ofList
    |> OSet.map Row.make

let cells (length : size) : OSet<cell> =
    let columns' = columns length in

    rows length
    |> OSet.map
        (fun row ->
            columns'
            |> OSet.map
                (fun column -> Cell.make column row))
    |> OSet.concat

let stacks (length : size) (boxWidth : boxWidth) : OSet<stack> =
    Sset.range 1 (length / boxWidth)
    |> OSet.ofList
    |> OSet.map Stack.make

let bands (length : size) (boxHeight : boxHeight) : OSet<band> =
    Sset.range 1 (length / boxHeight)
    |> OSet.ofList
    |> OSet.map Band.make

let boxes (length : size) (boxWidth : boxWidth) (boxHeight : boxHeight) : OSet<box> =
    let stacks' = stacks length boxWidth in

    bands length boxHeight
    |> OSet.map
        (fun band ->
          stacks'
          |> OSet.map (fun stack -> Box.make stack band))
    |>OSet.concat

let houses (length : size) (boxWidth : boxWidth) (boxHeight : boxHeight) : OSet<house> =
    let chs = OSet.map House.make_column (columns length) in
    let rhs = OSet.map House.make_row (rows length) in
    let bhs = OSet.map House.make_box (boxes length boxWidth boxHeight) in

    OSet.unionMany [ chs; rhs; bhs ]

let columnCells (length : size) (column : column) : OSet<cell> =
    rows length
    |> OSet.map
        (fun row -> Cell.make column row)

let rowCells (length : size) (row : row) : OSet<cell> =
    columns length
    |> OSet.map
        (fun column -> Cell.make column row)

let columnStack (boxWidth : boxWidth) (column : column) : stack =
    match column with
    | CColumn c ->
        1 + (c - 1) / boxWidth
        |> Stack.make

let stackColumns (boxWidth : boxWidth) (stack : stack) : OSet<column> =
    match stack with
    | SStack s ->
        let t = (s - 1) * boxWidth in
        Sset.range (t + 1) (t + boxWidth)
        |> OSet.ofList
        |> OSet.map Column.make

let rowBand (boxHeight : boxHeight) (row : row) : band =
    match row with
    | RRow r ->
        1 + (r - 1) / boxHeight
        |> Band.make

let bandRows (boxHeight : boxHeight) (band : band) : OSet<row> =
    let c = match band with BBand b -> (b - 1) * boxHeight in

    Sset.range (c + 1) (c + boxHeight)
    |> OSet.ofList
    |> OSet.map Row.make

let cellBox (boxWidth : boxWidth) (boxHeight : boxHeight) (cell : cell) : box =
    let stack = columnStack boxWidth cell.col in
    let band = rowBand boxHeight cell.row in
    Box.make stack band

let boxCells (boxWidth : boxWidth) (boxHeight : boxHeight) (box : box) : OSet<cell> =
    let stackColumns = stackColumns boxWidth box.stack in
    let bandRows = bandRows boxHeight box.band in

    bandRows
    |> OSet.map
        (fun row ->
            stackColumns
            |> OSet.map (fun column -> Cell.make column row))
    |> OSet.concat

let houseCells (length : size) (boxWidth : boxWidth) (boxHeight : boxHeight) (house : house) : cells =
    match house with
    | HColumn c -> columnCells length c
    | HRow r -> rowCells length r
    | HBox b -> boxCells boxWidth boxHeight b

let cellHouseCells (length : size) (boxWidth : boxWidth) (boxHeight : boxHeight) (cell : cell) : cells =
    let rowCells' =
        rowCells length cell.row
        |> OSet.remove cell in

    let columnCells' =
        columnCells length cell.col
        |> OSet.remove cell in

    let boxCells' =
        boxCells boxWidth boxHeight (cellBox boxWidth boxHeight cell)
        |> OSet.remove cell in

    [ OSet.singleton cell; rowCells'; columnCells'; boxCells' ]
    |> OSet.unionMany

type puzzleMap =
    {
        columns : columns;
        rows : rows;
        cells : cells;
        stacks : OSet<stack>;
        bands : OSet<band>;
        boxes : OSet<box>;
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
        cellBox : SMap<cell, box>;
        (* for a box, return the cells in it *)
        boxCells : SMap<box, cells>;
        (* for a house, return the cells in it *)
        houseCells : SMap<house, cells>;
        cellHouseCells : SMap<cell, cells>;
        housesCells : houses -> cells;
        houseCellCandidateReductions : house -> cellCandidates -> OSet<candidateReduction>;

        (*abstract member houseCellCandidates : (house, cellCandidates>*)
    }

let tPuzzleMap (puzzleShape : puzzleShape) : puzzleMap =

    let _columns = columns puzzleShape.size in
    let _rows = rows puzzleShape.size in
    let _cells = cells puzzleShape.size in
    let _stacks = stacks puzzleShape.size puzzleShape.boxWidth in
    let _bands = bands puzzleShape.size puzzleShape.boxHeight in
    let _boxes = boxes puzzleShape.size puzzleShape.boxWidth puzzleShape.boxHeight in
    let _houses = houses puzzleShape.size puzzleShape.boxWidth puzzleShape.boxHeight in
    let _columnCells = fun column -> columnCells puzzleShape.size column in
    let _rowCells = fun row -> rowCells puzzleShape.size row in
    let _columnStack = columnStack puzzleShape.boxWidth in
    let _stackColumns = stackColumns puzzleShape.boxWidth in
    let _rowBand = rowBand puzzleShape.boxHeight in
    let _bandRows = bandRows puzzleShape.boxHeight in
    let _cellBox = cellBox puzzleShape.boxWidth puzzleShape.boxHeight in
    let _boxCells = fun box -> boxCells puzzleShape.boxWidth puzzleShape.boxHeight box in
    let _houseCells = houseCells puzzleShape.size puzzleShape.boxWidth puzzleShape.boxHeight in
    let _cellHouseCells = cellHouseCells puzzleShape.size puzzleShape.boxWidth puzzleShape.boxHeight in

    let _columnCellsLookup = SMap.ofLookup _columnCells _columns in
    let _rowCellsLookup = SMap.ofLookup _rowCells _rows in
    let _columnStackLookup = SMap.ofLookup _columnStack _columns in
    let _stackColumnsLookup = SMap.ofLookup _stackColumns _stacks in
    let _rowBandLookup = SMap.ofLookup _rowBand _rows in
    let _bandRowsLookup = SMap.ofLookup _bandRows _bands in
    let _cellBoxLookup = SMap.ofLookup _cellBox _cells in
    let _boxCellsLookup = SMap.ofLookup _boxCells _boxes in
    let _houseCellsLookup = SMap.ofLookup _houseCells _houses in
    let _cellHouseCellsLookup = SMap.ofLookup _cellHouseCells _cells in

    let _housesCells (houses : houses) : cells =
        houses
        |> OSet.map (fun house -> SMap.get house _houseCellsLookup)
        |> OSet.concat in

    let _houseCellCandidateReductions (house : house) (cellCandidates : cellCandidates) : OSet<candidateReduction> =
        |> OSet.map (fun cell -> CandidateReduction.make cell (SMap.get cell cellCandidates)) in

    {
        columns = _columns;
        rows = _rows;
        cells = _cells;
        stacks = _stacks;
        bands = _bands;
        boxes = _boxes;
        houses = _houses;
        columnCells = _columnCellsLookup;
        rowCells = _rowCellsLookup;
        columnStack = _columnStackLookup;
        stackColumns = _stackColumnsLookup;
        rowBand = _rowBandLookup;
        bandRows = _bandRowsLookup;
        cellBox = _cellBoxLookup;
        boxCells = _boxCellsLookup;
        houseCells = _houseCellsLookup;
        cellHouseCells = _cellHouseCellsLookup;
        housesCells = _housesCells;
        houseCellCandidateReductions = _houseCellCandidateReductions;
    }
