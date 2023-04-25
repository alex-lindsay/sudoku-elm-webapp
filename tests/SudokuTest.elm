module SudokuTest exposing (..)

import Array exposing (initialize)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
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
            (\_ -> Expect.equal True (validPosition ( 1, 1 )))
        , test "valid positions 1,2 is valid"
            (\_ -> Expect.equal True (validPosition ( 1, 2 )))
        , test "valid positions 2,1 is valid"
            (\_ -> Expect.equal True (validPosition ( 2, 1 )))
        , test "valid positions 9,9 is valid"
            (\_ -> Expect.equal True (validPosition ( 9, 9 )))
        , test "invalid row 0,5 is invalid"
            (\_ -> Expect.equal False (validPosition ( 0, 5 )))
        , test "invalid col 5,0 is invalid"
            (\_ -> Expect.equal False (validPosition ( 5, 0 )))
        , test "overlarge row 10,5 is invalid"
            (\_ -> Expect.equal False (validPosition ( 10, 5 )))
        , test "overlarge col 5,10 is invalid"
            (\_ -> Expect.equal False (validPosition ( 5, 10 )))
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

