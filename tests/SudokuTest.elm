module SudokuTest exposing (..)

-- import Fuzz exposing (Fuzzer, int, list, string)

import Array exposing (..)
import CellHelpers exposing (..)
import Constants exposing (..)
import Expect exposing (..)
import Helpers exposing (..)
import List exposing (..)
import Set exposing (..)
import Sudoku exposing (..)
import SudokuTypes exposing (..)
import Test exposing (..)
import Updaters exposing (..)
import WinningStatus exposing (..)


sampleKnowns : Array Int
sampleKnowns =
    "020000000006037200000069080203700040005040006900020800000000051090000000014003000"
        |> String.split ""
        |> List.map (\s -> String.toInt s |> Maybe.withDefault 0)
        |> Array.fromList


sampleGuesses : Array Int
sampleGuesses =
    "000000000050000000000000000000000000000000000000000000000000000000000000000000000"
        |> String.split ""
        |> List.map (\s -> String.toInt s |> Maybe.withDefault 0)
        |> Array.fromList


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
            { pos = ( 1, 1 ), block = 1, value = Nothing, isVisible = False, guess = Nothing, marks = [] }

        newCell35 =
            { pos = ( 3, 5 ), block = 2, value = Nothing, isVisible = False, guess = Nothing, marks = [] }

        newCell97 =
            { pos = ( 9, 7), block = 9, value = Nothing, isVisible = False, guess = Nothing, marks = [] }
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
            (\_ ->
                Expect.equal
                    ( { gameState = Just SetKnown
                      , activeNumber = Just 1
                      , cells = Array.initialize 81 (\i -> newCellAt (indexToPosition i))

                      -- , cells = almostWinningBoard
                      , selectedPos = ( 1, 1 )
                      , winningStatus = Unknown
                      , autoSolveState = NotSolving
                      }
                    , Cmd.none
                    )
                    (init ())
            )
        ]


cellGuessOrValueTest : Test
cellGuessOrValueTest =
    let
        cellWithGuess =
            { pos = (1, 1), block = 1, value = Nothing, isVisible = False, guess = Just 1, marks = [] }

        cellWithValue =
            { pos = (1, 1), block = 1, value = Just 1, isVisible = False, guess = Nothing, marks = [] }

        cellWithoutGuessOrValue =
            { pos = (1, 1), block = 1, value = Nothing, isVisible = False, guess = Nothing, marks = [] }

        cellWithGuessAndValue =
            -- shouldn't happen, but just in case
            { pos = (1, 1), block = 1, value = Just 1, isVisible = False, guess = Just 1, marks = [] }
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


cellGuessOrKnownTest : Test
cellGuessOrKnownTest =
    let
        cellWithGuess =
            { pos = (1, 1), block = 1, value = Nothing, isVisible = False, guess = Just 1, marks = [] }

        cellWithValue =
            { pos = (1, 1), block = 1, value = Just 1, isVisible = False, guess = Nothing, marks = [] }

        cellWithKnown =
            { pos = (1, 1), block = 1, value = Just 1, isVisible = True, guess = Nothing, marks = [] }

        cellWithoutGuessOrValue =
            { pos = (1, 1), block = 1, value = Nothing, isVisible = False, guess = Nothing, marks = [] }

        cellWithGuessAndValue =
            -- shouldn't happen, but just in case
            { pos = (1, 1), block = 1, value = Just 1, isVisible = False, guess = Just 5, marks = [] }
    in
    describe "cellGuessOrKnownValue"
        [ test "cell with guess 1 returns 1"
            (\_ -> Expect.equal (Just 1) (cellGuessOrKnown cellWithGuess))
        , test "cell with invisible value 1 returns Nothing"
            (\_ -> Expect.equal Nothing (cellGuessOrKnown cellWithValue))
        , test "cell with known 1 returns 1"
            (\_ -> Expect.equal (Just 1) (cellGuessOrKnown cellWithKnown))
        , test "cell without guess or value returns Nothing"
            (\_ -> Expect.equal Nothing (cellGuessOrKnown cellWithoutGuessOrValue))
        , test "cell with guess and value returns guess"
            (\_ -> Expect.equal (Just 5) (cellGuessOrKnown cellWithGuessAndValue))
        ]


rowCellsTest : Test
rowCellsTest =
    let
        ( start, _ ) =
            init ()

        model =
            { start | cells = almostWinningBoard }

        actualResults =
            List.map (\i -> rowCells i model) digits

        actualLengths =
            List.map (\row -> List.length (Array.toList row)) actualResults

        actualRows =
            actualResults
                |> List.map (\row -> List.map (\cell -> (cellRow cell)) (Array.toList row))
    in
    describe "rowCells"
        [ test "lengths are as expected"
            (\_ -> Expect.equal (List.map (\_ -> 9) (List.range 1 9)) actualLengths)
        , test "rows are as expected"
            (\_ -> Expect.equal (List.map (\i -> List.repeat 9 i) (List.range 1 9)) actualRows)
        ]


colCellsTest : Test
colCellsTest =
    let
        ( start, _ ) =
            init ()

        model =
            { start | cells = almostWinningBoard }

        actualResults =
            List.map (\i -> colCells i model) digits

        actualLengths =
            List.map (\col -> List.length (Array.toList col)) actualResults

        actualCols =
            actualResults
                |> List.map (\col -> List.map (\cell -> (cellCol cell)) (Array.toList col))
    in
    describe "colCells"
        [ test "lengths are as expected"
            (\_ -> Expect.equal (List.map (\_ -> 9) (List.range 1 9)) actualLengths)
        , test "cols are as expected"
            (\_ -> Expect.equal (List.map (\i -> List.repeat 9 i) (List.range 1 9)) actualCols)
        ]


blockCellsTest : Test
blockCellsTest =
    let
        ( start, _ ) =
            init ()

        model =
            { start | cells = almostWinningBoard }

        actualResults =
            List.map (\i -> blockCells i model) (List.range 1 9)

        actualLengths =
            List.map (\block -> List.length (Array.toList block)) actualResults

        actualBlocks =
            List.map (\block -> List.map (\cell -> cell.block) (Array.toList block)) actualResults
    in
    describe "blockCells"
        [ test "lengths are as expected"
            (\_ -> Expect.equal (List.map (\_ -> 9) (List.range 1 9)) actualLengths)
        , test "blocks are as expected"
            (\_ -> Expect.equal (List.map (\i -> List.repeat 9 i) (List.range 1 9)) actualBlocks)
        ]


hasNumberRepeatedTest : Test
hasNumberRepeatedTest =
    describe "hasNumberRepeated"
        [ test "no numbers are repeated"
            (\_ -> Expect.equal False (hasNumberRepeated [ 1, 2, 3, 4, 5, 6, 7, 8, 9 ]))
        , test "numbers are repeated"
            (\_ -> Expect.equal True (hasNumberRepeated [ 1, 2, 3, 4, 5, 6, 7, 8, 1 ]))
        , test "empty list edge case"
            (\_ -> Expect.equal False (hasNumberRepeated []))
        ]


anyRowHasValueRepeatedTest : Test
anyRowHasValueRepeatedTest =
    let
        ( start, _ ) =
            init ()

        model =
            { start | cells = almostWinningBoard }

        modelWithRepeats =
            { model
                | cells =
                    Array.map
                        (\cell ->
                            { cell
                                | value =
                                    case
                                        cell.value
                                    of
                                        Nothing ->
                                            Just 1

                                        _ ->
                                            cell.value
                            }
                        )
                        model.cells
            }
    in
    describe "anyRowHasValueRepeated"
        [ test "no numbers are repeated"
            (\_ -> Expect.equal False (anyRowHasValueRepeated model))
        , test "numbers are repeated"
            (\_ -> Expect.equal True (anyRowHasValueRepeated modelWithRepeats))
        , test "empty list edge case"
            (\_ -> Expect.equal False (anyRowHasValueRepeated { model | cells = Array.fromList [] }))
        ]


anyRowHasGuessRepeatedTest : Test
anyRowHasGuessRepeatedTest =
    let
        ( start, _ ) =
            init ()

        model =
            { start | cells = almostWinningBoard }

        modelWithRepeats =
            { model
                | cells =
                    Array.map
                        (\cell ->
                            { cell
                                | guess =
                                    case
                                        cell.guess
                                    of
                                        Nothing ->
                                            Just 2

                                        _ ->
                                            cell.guess
                            }
                        )
                        model.cells
            }
    in
    describe "anyRowHasGuessRepeated"
        [ test "no numbers are repeated"
            (\_ -> Expect.equal False (anyRowHasGuessRepeated model))
        , test "numbers are repeated"
            (\_ -> Expect.equal True (anyRowHasGuessRepeated modelWithRepeats))
        , test "empty list edge case"
            (\_ -> Expect.equal False (anyRowHasGuessRepeated { model | cells = Array.fromList [] }))
        ]


anyColHasValueRepeatedTest : Test
anyColHasValueRepeatedTest =
    let
        ( start, _ ) =
            init ()

        model =
            { start | cells = almostWinningBoard }

        modelWithRepeats =
            { model
                | cells =
                    Array.map
                        (\cell ->
                            { cell
                                | value =
                                    case
                                        cell.value
                                    of
                                        Nothing ->
                                            Just 1

                                        _ ->
                                            cell.value
                            }
                        )
                        model.cells
            }
    in
    describe "anyColHasValueRepeated"
        [ test "no numbers are repeated"
            (\_ -> Expect.equal False (anyColHasValueRepeated model))
        , test "numbers are repeated"
            (\_ -> Expect.equal True (anyColHasValueRepeated modelWithRepeats))
        , test "empty list edge case"
            (\_ -> Expect.equal False (anyColHasValueRepeated { model | cells = Array.fromList [] }))
        ]


anyColHasGuessRepeatedTest : Test
anyColHasGuessRepeatedTest =
    let
        ( start, _ ) =
            init ()

        model =
            { start | cells = almostWinningBoard }

        modelWithRepeats =
            { model
                | cells =
                    Array.map
                        (\cell ->
                            { cell
                                | guess =
                                    case
                                        cell.guess
                                    of
                                        Nothing ->
                                            Just 2

                                        _ ->
                                            cell.guess
                            }
                        )
                        model.cells
            }
    in
    describe "anyColHasGuessRepeated"
        [ test "no numbers are repeated"
            (\_ -> Expect.equal False (anyColHasGuessRepeated model))
        , test "numbers are repeated"
            (\_ -> Expect.equal True (anyColHasGuessRepeated modelWithRepeats))
        , test "empty list edge case"
            (\_ -> Expect.equal False (anyColHasGuessRepeated { model | cells = Array.fromList [] }))
        ]


anyBlockHasValueRepeatedTest : Test
anyBlockHasValueRepeatedTest =
    let
        ( start, _ ) =
            init ()

        model =
            { start | cells = almostWinningBoard }

        modelWithRepeats =
            { model
                | cells =
                    Array.map
                        (\cell ->
                            { cell
                                | value =
                                    case
                                        cell.value
                                    of
                                        Nothing ->
                                            Just 1

                                        _ ->
                                            cell.value
                            }
                        )
                        model.cells
            }
    in
    describe "anyBlockHasValueRepeated"
        [ test "no numbers are repeated"
            (\_ -> Expect.equal False (anyBlockHasValueRepeated model))
        , test "numbers are repeated"
            (\_ -> Expect.equal True (anyBlockHasValueRepeated modelWithRepeats))
        , test "empty list edge case"
            (\_ -> Expect.equal False (anyBlockHasValueRepeated { model | cells = Array.fromList [] }))
        ]


anyBlockHasGuessRepeatedTest : Test
anyBlockHasGuessRepeatedTest =
    let
        ( start, _ ) =
            init ()

        model =
            { start | cells = almostWinningBoard }

        modelWithRepeats =
            { model
                | cells =
                    Array.map
                        (\cell ->
                            { cell
                                | guess =
                                    case
                                        cell.guess
                                    of
                                        Nothing ->
                                            Just 2

                                        _ ->
                                            cell.guess
                            }
                        )
                        model.cells
            }
    in
    describe "anyBlockHasGuessRepeated"
        [ test "no numbers are repeated"
            (\_ -> Expect.equal False (anyBlockHasGuessRepeated model))
        , test "numbers are repeated"
            (\_ -> Expect.equal True (anyBlockHasGuessRepeated modelWithRepeats))
        , test "empty list edge case"
            (\_ -> Expect.equal False (anyBlockHasGuessRepeated { model | cells = Array.fromList [] }))
        ]


cellsAreCompleteTest : Test
cellsAreCompleteTest =
    let
        incompleteCells =
            Array.initialize 9 (\i -> newCellAt ( i, 0 ))

        completeCells =
            Array.indexedMap (\i cell -> { cell | value = Just (i + 1) }) incompleteCells
    in
    describe "cellsAreComplete"
        [ test "cells are not complete"
            (\_ -> Expect.equal False (cellsAreComplete incompleteCells .value))
        , test "cells are complete"
            (\_ -> Expect.equal True (cellsAreComplete completeCells .value))
        , test "empty list edge case"
            (\_ -> Expect.equal True (cellsAreComplete (Array.fromList []) .value))
        ]


blockIsCompleteTest : Test
blockIsCompleteTest =
    let
        ( start, _ ) =
            init ()

        model =
            { start | cells = almostWinningBoard }
    in
    describe "blockIsComplete"
        [ test "block is not complete"
            (\_ -> Expect.equal False (blockIsComplete 9 cellGuessOrValue model))
        , test "block is complete"
            (\_ -> Expect.equal True (blockIsComplete 1 cellGuessOrValue model))
        ]


allRowsAreCompleteTest : Test
allRowsAreCompleteTest =
    let
        ( start, _ ) =
            init ()

        model =
            { start | cells = almostWinningBoard }

        modelWithCompleteRows =
            { model
                | cells =
                    Array.map
                        (\cell ->
                            { cell
                                | value =
                                    case
                                        cell.value
                                    of
                                        Nothing ->
                                            Just 2

                                        _ ->
                                            cell.value
                            }
                        )
                        model.cells
            }
    in
    describe "allRowsAreComplete"
        [ test "rows are not complete"
            (\_ -> Expect.equal False (allRowsAreComplete model))
        , test "rows are complete"
            (\_ -> Expect.equal True (allRowsAreComplete modelWithCompleteRows))
        , test "empty list edge case"
            (\_ -> Expect.equal True (allRowsAreComplete { model | cells = Array.fromList [] }))
        ]


allColsAreCompleteTest : Test
allColsAreCompleteTest =
    let
        ( start, _ ) =
            init ()

        model =
            { start | cells = almostWinningBoard }

        modelWithCompleteCols =
            { model
                | cells =
                    Array.map
                        (\cell ->
                            { cell
                                | value =
                                    case
                                        cell.value
                                    of
                                        Nothing ->
                                            Just 2

                                        _ ->
                                            cell.value
                            }
                        )
                        model.cells
            }
    in
    describe "allColsAreComplete"
        [ test "cols are not complete"
            (\_ -> Expect.equal False (allColsAreComplete model))
        , test "cols are complete"
            (\_ -> Expect.equal True (allColsAreComplete modelWithCompleteCols))
        , test "empty list edge case"
            (\_ -> Expect.equal True (allColsAreComplete { model | cells = Array.fromList [] }))
        ]


allBlocksAreCompleteTest : Test
allBlocksAreCompleteTest =
    let
        ( start, _ ) =
            init ()

        model =
            { start | cells = almostWinningBoard }

        modelWithCompleteBlocks =
            { model
                | cells =
                    Array.map
                        (\cell ->
                            { cell
                                | value =
                                    case
                                        cell.value
                                    of
                                        Nothing ->
                                            Just 2

                                        _ ->
                                            cell.value
                            }
                        )
                        model.cells
            }
    in
    describe "allBlocksAreComplete"
        [ test "blocks are not complete"
            (\_ -> Expect.equal False (allBlocksAreComplete model))
        , test "blocks are complete"
            (\_ -> Expect.equal True (allBlocksAreComplete modelWithCompleteBlocks))
        , test "empty list edge case"
            (\_ -> Expect.equal True (allBlocksAreComplete { model | cells = Array.fromList [] }))
        ]


hasWinningStatusUnknownTest : Test
hasWinningStatusUnknownTest =
    let
        ( start, _ ) =
            init ()

        model =
            { start | cells = almostWinningBoard }

        modelWithCompleteBlocks =
            { model
                | cells =
                    Array.map
                        (\cell ->
                            { cell
                                | value =
                                    case
                                        cell.value
                                    of
                                        Nothing ->
                                            Just 2

                                        _ ->
                                            cell.value
                            }
                        )
                        model.cells
            }

        modelWithError =
            { model
                | cells =
                    Array.map
                        (\cell ->
                            { cell
                                | value =
                                    case
                                        cell.value
                                    of
                                        Nothing ->
                                            Just 1

                                        _ ->
                                            cell.value
                            }
                        )
                        model.cells
            }
    in
    describe "hasWinningStatusUnknown"
        [ test "status is unknown"
            (\_ -> Expect.equal True (hasWinningStatusUnknown model))
        , test "status should be winning"
            (\_ -> Expect.equal False (hasWinningStatusUnknown modelWithCompleteBlocks))
        , test "status should be losing"
            (\_ -> Expect.equal False (hasWinningStatusUnknown modelWithError))
        ]


hasWinningStatusWonTest : Test
hasWinningStatusWonTest =
    let
        ( start, _ ) =
            init ()

        model =
            { start | cells = almostWinningBoard }

        modelWithCompleteBlocks =
            { model
                | cells =
                    Array.map
                        (\cell ->
                            { cell
                                | value =
                                    case
                                        cell.value
                                    of
                                        Nothing ->
                                            Just 2

                                        _ ->
                                            cell.value
                            }
                        )
                        model.cells
            }

        modelWithError =
            { model
                | cells =
                    Array.map
                        (\cell ->
                            { cell
                                | value =
                                    case
                                        cell.value
                                    of
                                        Nothing ->
                                            Just 1

                                        _ ->
                                            cell.value
                            }
                        )
                        model.cells
            }
    in
    describe "hasWinningStatusWon"
        [ test "status is unknown"
            (\_ -> Expect.equal False (hasWinningStatusWon model))
        , test "status should be winning"
            (\_ -> Expect.equal True (hasWinningStatusWon modelWithCompleteBlocks))
        , test "status should be losing"
            (\_ -> Expect.equal False (hasWinningStatusWon modelWithError))
        ]


hasWinningStatusLostTest : Test
hasWinningStatusLostTest =
    let
        ( start, _ ) =
            init ()

        model =
            { start | cells = almostWinningBoard }

        modelWithCompleteBlocks =
            { model
                | cells =
                    Array.map
                        (\cell ->
                            { cell
                                | value =
                                    case
                                        cell.value
                                    of
                                        Nothing ->
                                            Just 2

                                        _ ->
                                            cell.value
                            }
                        )
                        model.cells
            }

        modelWithError =
            { model
                | cells =
                    Array.map
                        (\cell ->
                            { cell
                                | value =
                                    case
                                        cell.value
                                    of
                                        Nothing ->
                                            Just 1

                                        _ ->
                                            cell.value
                            }
                        )
                        model.cells
            }
    in
    describe "hasWinningStatusLost"
        [ test "status is unknown"
            (\_ -> Expect.equal False (hasWinningStatusLost model))
        , test "status should be winning"
            (\_ -> Expect.equal False (hasWinningStatusLost modelWithCompleteBlocks))
        , test "status should be losing"
            (\_ -> Expect.equal True (hasWinningStatusLost modelWithError))
        ]


updateWinningStatusTest : Test
updateWinningStatusTest =
    let
        ( start, _ ) =
            init ()

        model =
            { start | cells = almostWinningBoard }

        modelWithCompleteBlocks =
            { model
                | cells =
                    Array.map
                        (\cell ->
                            { cell
                                | value =
                                    case
                                        cell.value
                                    of
                                        Nothing ->
                                            Just 2

                                        _ ->
                                            cell.value
                            }
                        )
                        model.cells
            }

        modelWithError =
            { model
                | cells =
                    Array.map
                        (\cell ->
                            { cell
                                | value =
                                    case
                                        cell.value
                                    of
                                        Nothing ->
                                            Just 1

                                        _ ->
                                            cell.value
                            }
                        )
                        model.cells
            }
    in
    describe "updateWinningStatus"
        [ test "status is unknown"
            (\_ -> Expect.equal Unknown (updateWinningStatus model).winningStatus)
        , test "status should be winning"
            (\_ -> Expect.equal Won (updateWinningStatus modelWithCompleteBlocks).winningStatus)
        , test "status should be losing"
            (\_ -> Expect.equal Lost (updateWinningStatus modelWithError).winningStatus)
        ]


guessesAndKnownsForCellsTest : Test
guessesAndKnownsForCellsTest =
    let
        cellWithGuess11 =
            (\cell -> { cell | value = Nothing, guess = Just 1 }) <| newCellAt ( 1, 1 )

        cellWithGuess12 =
            (\cell -> { cell | value = Nothing, guess = Just 2 }) <| newCellAt ( 1, 2 )

        cellWithKnown13 =
            (\cell -> { cell | value = Just 3, isVisible = True }) <| newCellAt ( 1, 3 )

        cellWithKnown14 =
            (\cell -> { cell | value = Just 4, isVisible = True }) <| newCellAt ( 1, 4 )

        cellWithValue15 =
            (\cell -> { cell | value = Just 5, isVisible = False }) <| newCellAt ( 1, 5 )

        cellWithValue16 =
            (\cell -> { cell | value = Just 6, isVisible = False }) <| newCellAt ( 1, 6 )

        cellWithNone17 =
            newCellAt ( 1, 7 )

        cellWithNone18 =
            newCellAt ( 1, 8 )

        cellWithNone19 =
            newCellAt ( 1, 9 )

        cells =
            Array.fromList [ cellWithGuess11, cellWithGuess12, cellWithKnown13, cellWithKnown14, cellWithValue15, cellWithValue16, cellWithNone17, cellWithNone18, cellWithNone19 ]
    in
    describe "guessesAndKnownsForCells"
        [ test "guesses and knowns"
            (\_ -> Expect.equalLists [ 1, 2, 3, 4 ] (guessesAndKnownsForCells cells))
        , test "empty list edge case"
            (\_ -> Expect.equal [] (guessesAndKnownsForCells (Array.fromList [])))
        ]


guessesAndKnownsForCellAtTest : Test
guessesAndKnownsForCellAtTest =
    let
        ( model1, _ ) =
            init ()

        cells =
            emptyBoard
                |> Array.indexedMap
                    (\i cell ->
                        { cell
                            | value =
                                case
                                    Array.get i sampleKnowns
                                of
                                    Just 0 ->
                                        Nothing

                                    _ ->
                                        Array.get i sampleKnowns
                            , isVisible =
                                case
                                    Array.get i sampleKnowns
                                of
                                    Just 0 ->
                                        False

                                    _ ->
                                        True
                            , guess =
                                case
                                    Array.get i sampleGuesses
                                of
                                    Just 0 ->
                                        Nothing

                                    _ ->
                                        Array.get i sampleGuesses
                        }
                    )

        model =
            { model1 | cells = cells }
    in
    describe "guessesAndKnownsForCellAt"
        [ test "at cell(1,1)"
            (\_ -> Expect.equalSets (Set.fromList [ 2, 6, 5, 9 ]) (Set.fromList (guessesAndKnownsForCellAt ( 1, 1 ) model)))
        , test "at cell(2,1)"
            (\_ -> Expect.equalSets (Set.fromList [ 2, 3, 5, 6, 7, 9 ]) (Set.fromList (guessesAndKnownsForCellAt ( 2, 1 ) model)))
        , test "at cell(1,2)"
            (\_ -> Expect.equalSets (Set.fromList [ 2, 6, 5, 9, 1 ]) (Set.fromList (guessesAndKnownsForCellAt ( 1, 2 ) model)))
        , test "at cell(9,9)"
            (\_ -> Expect.equalSets (Set.fromList [ 1, 5, 6, 3, 4 ]) (Set.fromList (guessesAndKnownsForCellAt ( 9, 9 ) model)))
        ]


autoHintsForCellAtTest : Test
autoHintsForCellAtTest =
    let
        ( model1, _ ) =
            init ()

        cells =
            emptyBoard
                |> Array.indexedMap
                    (\i cell ->
                        { cell
                            | value =
                                case
                                    Array.get i sampleKnowns
                                of
                                    Just 0 ->
                                        Nothing

                                    _ ->
                                        Array.get i sampleKnowns
                            , isVisible =
                                case
                                    Array.get i sampleKnowns
                                of
                                    Just 0 ->
                                        False

                                    _ ->
                                        True
                            , guess =
                                case
                                    Array.get i sampleGuesses
                                of
                                    Just 0 ->
                                        Nothing

                                    _ ->
                                        Array.get i sampleGuesses
                        }
                    )

        model =
            { model1 | cells = cells }
    in
    describe "autoHintsForCellAt"
        [ test "at cell(1,1)"
            (\_ -> Expect.equalSets (Set.fromList [ 1, 3, 4, 7, 8 ]) (Set.fromList (autoHintsForCellAt ( 1, 1 ) model)))
        , test "at cell(2,1)"
            (\_ -> Expect.equalSets (Set.fromList [ 1, 4, 8 ]) (Set.fromList (autoHintsForCellAt ( 2, 1 ) model)))
        , test "at cell(1,2)"
            (\_ -> Expect.equalSets (Set.fromList [ 3, 4, 7, 8 ]) (Set.fromList (autoHintsForCellAt ( 1, 2 ) model)))
        , test "at cell(9,9)"
            (\_ -> Expect.equalSets (Set.fromList [ 2, 7, 8, 9 ]) (Set.fromList (autoHintsForCellAt ( 9, 9 ) model)))
        ]
