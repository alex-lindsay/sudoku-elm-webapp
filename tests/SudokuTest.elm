module SudokuTest exposing (..)

import Array exposing (initialize)
import Expect exposing (..)
import List exposing (..)
-- import Fuzz exposing (Fuzzer, int, list, string)
import Sudoku exposing (..)
import Test exposing (..)



validIndexTest : Test
validIndexTest =
    describe "validIndex"
        [ test "valid index 0 is valid"
            (\_ -> Expect.equal True (validIndex 0))
        , test "valid index 1 is valid"
            (\_ -> Expect.equal True (validIndex 1))
        , test "valid index 9 is valid"
            (\_ -> Expect.equal True (validIndex 9))
        , test "valid index 80 is valid"
            (\_ -> Expect.equal True (validIndex 80))
        , test "negative index is invalid"
            (\_ -> Expect.equal False (validIndex -1))
        , test "overlarge index is invalid"
            (\_ -> Expect.equal False (validIndex 81))
        ]


validPositionTest : Test
validPositionTest =
    describe "validPosition"
        [ test "valid positions 1,1 is valid"
            (\_ -> Expect.equal True <| validPosition ( 1, 1 ))
        , test "valid positions 1,2 is valid"
            (\_ -> Expect.equal True <| validPosition ( 1, 2 ))
        , test "valid positions 2,1 is valid"
            (\_ -> Expect.equal True <| validPosition ( 2, 1 ))
        , test "valid positions 9,9 is valid"
            (\_ -> Expect.equal True <| validPosition ( 9, 9 ))
        , test "invalid row 0,5 is invalid"
            (\_ -> Expect.equal False <| validPosition ( 0, 5 ))
        , test "invalid col 5,0 is invalid"
            (\_ -> Expect.equal False <| validPosition ( 5, 0 ))
        , test "overlarge row 10,5 is invalid"
            (\_ -> Expect.equal False <| validPosition ( 10, 5 ))
        , test "overlarge col 5,10 is invalid"
            (\_ -> Expect.equal False <| validPosition ( 5, 10 ))
        ]


indexToPositionTest : Test
indexToPositionTest =
    describe "indexToPosition"
        [ test "valid index 0 is converted to positions 1,1"
            (\_ -> Expect.equal ( 1, 1 ) (indexToPosition 0))
        , test "valid index 1 is converted to positions 1,2"
            (\_ -> Expect.equal ( 1, 2 ) (indexToPosition 1))
        , test "valid index 9 is  converted to positions 2,1"
            (\_ -> Expect.equal ( 2, 1 ) (indexToPosition 9))
        , test "valid index 80 is converted to positions 9,9"
            (\_ -> Expect.equal ( 9, 9 ) (indexToPosition 80))
        , test "negative index is converted to positions 0,0"
            (\_ -> Expect.equal ( 0, 0 ) (indexToPosition -1))
        , test "overlarge index is converted to positions 0,0"
            (\_ -> Expect.equal ( 0, 0 ) (indexToPosition 81))
        ]


positionToIndexTest : Test
positionToIndexTest =
    describe "positionToIndex"
        [ test "valid positions 1,1 is converted to index 0"
            (\_ -> Expect.equal 0 (positionToIndex ( 1, 1 )))
        , test "valid positions 1,2 is converted to index 1"
            (\_ -> Expect.equal 1 (positionToIndex ( 1, 2 )))
        , test "valid positions 2,1 is converted to index 9"
            (\_ -> Expect.equal 9 (positionToIndex ( 2, 1 )))
        , test "valid positions 9,9 is converted to index 80"
            (\_ -> Expect.equal 80 (positionToIndex ( 9, 9 )))
        , test "negative positions 0,0 is converted to index -1"
            (\_ -> Expect.equal -1 (positionToIndex ( 0, 0 )))
        ]


newCellAtTest : Test
newCellAtTest =
    -- TODO: we really shouldn't return a default cell with invalid positions, but we do for now
    let
        newCell11 =
            { row = 1, col = 1, block = 1, value = Nothing, isVisible = False, guess = Nothing, marks = [] }
        newCell35 =
            { row = 3, col = 5, block = 2, value = Nothing, isVisible = False, guess = Nothing, marks = [] }
        newCell97 =
            { row = 9, col = 7, block = 9, value = Nothing, isVisible = False, guess = Nothing, marks = [] }
    in
    describe "newCellAt"
        [ test "new cell at (1,1) is as expected"
            (\_ -> Expect.equal newCell11 (newCellAt ( 1, 1 )))
        , test "new cell at (3,5) is as expected"
            (\_ -> Expect.equal newCell35 (newCellAt ( 3, 5 )))
        , test "new cell at (9,7) is as expected"
            (\_ -> Expect.equal newCell97 (newCellAt ( 9, 7 )))
        , test "new invalid cell at (20,20) is equal to (1,1)"
            (\_ -> Expect.equal newCell11 (newCellAt ( 20, 20 )))
        ]


initTest : Test
initTest =
    describe "init"
        [ test "init is as expected"
            (\_ -> Expect.equal ({
                gameState = Just SetAnswer
                , activeNumber = Just 1
                , cells = initialize 81 (\i -> newCellAt (indexToPosition i))
                -- , cells = almostWinningBoard
                , selectedCell = Nothing
                , winningStatus = Unknown
                }
            , Cmd.none) (init))
        ]


cellValueTest : Test
cellValueTest =
    let
        cellWithValue =
            { row = 1, col = 1, block = 1, value = Just 1, isVisible = False, guess = Nothing, marks = [] }
        cellWithoutValue =
            { row = 1, col = 1, block = 1, value = Nothing, isVisible = False, guess = Nothing, marks = [] }
    in
    describe "cellValue"
        [ test "cell with value 1 returns 1"
            (\_ -> Expect.equal (Just 1) (cellValue cellWithValue))
        , test "cell without value returns Nothing"
            (\_ -> Expect.equal Nothing (cellValue cellWithoutValue))
        ]


cellGuessTest : Test
cellGuessTest =
    let
        cellWithGuess =
            { row = 1, col = 1, block = 1, value = Nothing, isVisible = False, guess = Just 1, marks = [] }
        cellWithoutGuess =
            { row = 1, col = 1, block = 1, value = Nothing, isVisible = False, guess = Nothing, marks = [] }
    in
    describe "cellGuess"
        [ test "cell with guess 1 returns 1"
            (\_ -> Expect.equal (Just 1) (cellGuess cellWithGuess))
        , test "cell without guess returns Nothing"
            (\_ -> Expect.equal Nothing (cellGuess cellWithoutGuess))
        ]


cellGuessOrValueTest : Test
cellGuessOrValueTest =
    let
        cellWithGuess =
            { row = 1, col = 1, block = 1, value = Nothing, isVisible = False, guess = Just 1, marks = [] }
        cellWithValue =
            { row = 1, col = 1, block = 1, value = Just 1, isVisible = False, guess = Nothing, marks = [] }
        cellWithoutGuessOrValue =
            { row = 1, col = 1, block = 1, value = Nothing, isVisible = False, guess = Nothing, marks = [] }
        cellWithGuessAndValue = -- shouldn't happen, but just in case
            { row = 1, col = 1, block = 1, value = Just 1, isVisible = False, guess = Just 1, marks = [] } 
    in
    describe "cellGuessOrValue"
        [ test "cell with guess 1 returns 1"
            (\_ -> Expect.equal (Just 1) (cellGuessOrValue cellWithGuess))
        , test "cell with value 1 returns 1"
            (\_ -> Expect.equal (Just 1) (cellGuessOrValue cellWithValue))
        , test "cell without guess or value returns Nothing"
            (\_ -> Expect.equal Nothing (cellGuessOrValue cellWithoutGuessOrValue))
        , test "cell with guess and value returns value"
            (\_ -> Expect.equal (Just 1) (cellGuessOrValue cellWithGuessAndValue))
        ]

rowCellsTest : Test
rowCellsTest = 
    let
        (start, _) = init
        model = {start | cells = almostWinningBoard}
        actualResults = List.map (\i -> (rowCells i model)) (List.range 1 9)
        actualLengths = List.map (\row -> (List.length row)) actualResults
        actualRows = List.map (\row -> (List.map (\cell -> cell.row) row)) actualResults
    in
    describe "rowCells"
        [test "lengths are as expected"
            (\_ -> Expect.equal (List.map (\_ -> 9) (List.range 1 9)) actualLengths)
        , test "rows are as expected"
            (\_ -> Expect.equal (List.map (\i -> List.repeat 9 i) (List.range 1 9)) actualRows)
        ]

colCellsTest : Test
colCellsTest = 
    let
        (start, _) = init
        model = {start | cells = almostWinningBoard}
        actualResults = List.map (\i -> (colCells i model)) (List.range 1 9)
        actualLengths = List.map (\col -> (List.length col)) actualResults
        actualCols = List.map (\col -> (List.map (\cell -> cell.col) col)) actualResults
    in
    describe "colCells"
        [test "lengths are as expected"
            (\_ -> Expect.equal (List.map (\_ -> 9) (List.range 1 9)) actualLengths)
        , test "cols are as expected"
            (\_ -> Expect.equal (List.map (\i -> List.repeat 9 i) (List.range 1 9)) actualCols)
        ]

blockCellsTest : Test
blockCellsTest = 
    let
        (start, _) = init
        model = {start | cells = almostWinningBoard}
        actualResults = List.map (\i -> (blockCells i model)) (List.range 1 9)
        actualLengths = List.map (\block -> (List.length block)) actualResults
        actualBlocks = List.map (\block -> (List.map (\cell -> cell.block) block)) actualResults
    in
    describe "blockCells"
        [test "lengths are as expected"
            (\_ -> Expect.equal (List.map (\_ -> 9) (List.range 1 9)) actualLengths)
        , test "blocks are as expected"
            (\_ -> Expect.equal (List.map (\i -> List.repeat 9 i) (List.range 1 9)) actualBlocks)
        ]

hasNumberRepeatedTest : Test
hasNumberRepeatedTest = 
    describe "hasNumberRepeated"
        [test "no numbers are repeated"
            (\_ -> Expect.equal False (hasNumberRepeated [1,2,3,4,5,6,7,8,9]))
            , test "numbers are repeated"
            (\_ -> Expect.equal True (hasNumberRepeated [1,2,3,4,5,6,7,8,1]))
            , test "empty list edge case"
            (\_ -> Expect.equal False (hasNumberRepeated []))
        ]

anyRowHasValueRepeatedTest : Test
anyRowHasValueRepeatedTest = 
    let
        (start, _) = init
        model = {start | cells = almostWinningBoard}
        modelWithRepeats = { model | cells = Array.map (\cell -> {cell | value = case 
            cell.value of 
                Nothing -> Just 1 
                _ -> cell.value
            }) model.cells }
        
    in
    describe "anyRowHasValueRepeated"
        [test "no numbers are repeated"
            (\_ -> Expect.equal False (anyRowHasValueRepeated model))
        , test "numbers are repeated"
            (\_ -> Expect.equal True (anyRowHasValueRepeated modelWithRepeats))
        , test "empty list edge case"
            (\_ -> Expect.equal False (anyRowHasValueRepeated {model | cells = Array.fromList []}))
        ]

anyRowHasGuessRepeatedTest : Test
anyRowHasGuessRepeatedTest = 
    let
        (start, _) = init
        model = {start | cells = almostWinningBoard}
        modelWithRepeats = { model | cells = Array.map (\cell -> {cell | guess = case 
            cell.guess of 
                Nothing -> Just 1 
                _ -> cell.guess
            }) model.cells }
        
    in
    describe "anyRowHasGuessRepeated"
        [test "no numbers are repeated"
            (\_ -> Expect.equal False (anyRowHasGuessRepeated model))
        , test "numbers are repeated"
            (\_ -> Expect.equal True (anyRowHasGuessRepeated modelWithRepeats))
        , test "empty list edge case"
            (\_ -> Expect.equal False (anyRowHasGuessRepeated {model | cells = Array.fromList []}))
        ]