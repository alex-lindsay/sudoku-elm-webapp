module HelpersTest exposing (..)

import Expect exposing (..)
import Helpers exposing (..)
import SudokuTypes exposing (..)
import Test exposing (..)


autoSolveStateToStringCases : List ( String, AutoSolveState, String )
autoSolveStateToStringCases = [
        ("Returns the correct string for a state with autoSolveState of NotSolving", NotSolving, "Not Solving")
        ,("Returns the correct string for a state with autoSolveState of CanceledSolving", CanceledSolving, "Canceled Solving")
        ,("Returns the correct string for a state with autoSolveState of CheckingFullHouse", CheckingFullHouse, "Checking Full House")
        ,("Returns the correct string for a state with autoSolveState of CheckingLastDigit", CheckingLastDigit, "Checking Last Digit")
        ,("Returns the correct string for a state with autoSolveState of CheckingHiddenSingle", CheckingHiddenSingle, "Checking Hidden Single")
        ,("Returns the correct string for a state with autoSolveState of CheckingPinnedDigit", CheckingPinnedDigit, "Checking Pinned Digit")
        ,("Returns the correct string for a state with autoSolveState of CheckingNakedSingle", CheckingNakedSingle, "Checking Naked Single")
        ,("Returns the correct string for a state with autoSolveState of CheckingForcedDigit", CheckingForcedDigit, "Checking Forced Digit")
        ,("Returns the correct string for a state with autoSolveState of CheckingSoleCandidate", CheckingSoleCandidate, "Checking Sole Candidate")
        ]

autoSolveStateToStringTest : Test
autoSolveStateToStringTest =
    autoSolveStateToStringCases
        |> List.map
            (\( description, autoSolveState, expected ) ->
                test description
                    (\_ -> Expect.equal
                        expected
                        (autoSolveStateToString autoSolveState)
                    )
            )
        |> describe "autoSolveStateToString"
       
            
hasNumberRepeatedCases : List ( String, List Int, Bool )
hasNumberRepeatedCases = [
    ("Returns true for a list with a repeated number", [1, 2, 3, 4, 5, 6, 7, 8, 8], True)
    ,("Returns true for a list with more than one repeated number", [1, 2, 3, 4, 4, 4, 7, 8, 8], True)
    ,("Returns false for a list without a repeated number", [1, 2, 3, 4, 5, 6, 7, 8, 9], False)
    ,("Returns false for an empty list", [], False)
    ]


hasNumberRepeatedTest : Test
hasNumberRepeatedTest =
    hasNumberRepeatedCases
        |> List.map
            (\( description, list, expected ) ->
                test description
                    (\_ -> Expect.equal
                        expected
                        (hasNumberRepeated list)
                    )
            )
        |> describe "hasNumberRepeated"


indexToPositionCases : List ( String, Int, Maybe Position )
indexToPositionCases = [
    ("Returns the correct position for index 0", 0, Just (1,1))
    ,("Returns the correct position for index 25", 25, Just (3,8))
    ,("Returns the correct position for index 80", 80, Just (9,9))
    ,("Returns Nothing for index -1", -1, Nothing)
    ,("Returns Nothing for index 81", 81, Nothing)
    ]


indexToPositionTest : Test
indexToPositionTest =
    indexToPositionCases
        |> List.map
            (\( description, index, expected ) ->
                test description
                    (\_ -> Expect.equal
                        expected
                        (indexToPosition index)
                    )
            )
        |> describe "indexToPosition"


newCellTemplate : Cell
newCellTemplate = { pos = (1, 1)
    , block = 1
    , value = Nothing
    , isVisible = False
    , guess = Nothing
    , marks = []
    }


newCellAtCases : List ( String, Position, Cell )
newCellAtCases = [
    ("Returns the correct cell for position (1,1)", (1,1), newCellTemplate)
    ,("Returns the correct cell for position (3,8)", (3,8), { newCellTemplate | pos = (3,8), block = 3 })
    ,("Returns the correct cell for position (4,5)", (4,5), { newCellTemplate | pos = (4,5), block = 5 })
    ,("Returns the correct cell for position (9,9)", (9,9), { newCellTemplate | pos = (9,9), block = 9 })
    ,("Returns the correct cell for garbage position (0,0)", (0,0), { newCellTemplate | pos = (1,1), block = 1 }) -- I don't know if this is the correct behavior (it should probably be Nothing)
    ]


newCellAtTest : Test
newCellAtTest =
    newCellAtCases
        |> List.map
            (\( description, position, expected ) ->
                test description
                    (\_ -> Expect.equal
                        expected
                        (newCellAt position)
                    )
            )
        |> describe "newCellAt"


positionToBlockCases : List ( String, Position, Int )
positionToBlockCases = [
    ("Returns the correct block for position (1,1)", (1,1), 1)
    ,("Returns the correct block for position (3,8)", (3,8), 3)
    ,("Returns the correct block for position (4,5)", (4,5), 5)
    ,("Returns the correct block for position (9,9)", (9,9), 9)
    ,("Returns the correct block for garbage position (0,0)", (0,0), -1) -- I don't know if this is the correct behavior (it should probably be Nothing)
    ,("Returns the correct block for garbage position (10,10)", (10,10), -1) -- I don't know if this is the correct behavior (it should probably be Nothing)
    ]


positionToBlockTest : Test
positionToBlockTest =
    positionToBlockCases
        |> List.map
            (\( description, position, expected ) ->
                test description
                    (\_ -> Expect.equal
                        expected
                        (positionToBlock position)
                    )
            )
        |> describe "positionToBlock"


positionToIndexCases : List ( String, Position, Int )
positionToIndexCases = [
    ("Returns the correct index for position (1,1)", (1,1), 0)
    ,("Returns the correct index for position (3,8)", (3,8), 25)
    ,("Returns the correct index for position (4,5)", (4,5), 31)
    ,("Returns the correct index for position (9,9)", (9,9), 80)
    ,("Returns the correct index for garbage position (0,0)", (0,0), -1) -- I don't know if this is the correct behavior (it should probably be Nothing)
    ,("Returns the correct index for garbage position (10,10)", (10,10), -1) -- I don't know if this is the correct behavior (it should probably be Nothing)
    ]


positionToIndexTest : Test
positionToIndexTest =
    positionToIndexCases
        |> List.map
            (\( description, position, expected ) ->
                test description
                    (\_ -> Expect.equal
                        expected
                        (positionToIndex position)
                    )
            )
        |> describe "positionToIndex"