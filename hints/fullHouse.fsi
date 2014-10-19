﻿module hints.fullHouse

open core.sudoku
open hints

val fullHousePerHouse : (Cell -> Set<Cell>)
     -> (House -> Set<Cell>) -> (Cell -> Set<Digit>) -> House -> HintDescription2 list
