module Helpers exposing (..)

import List exposing (..)
import Set exposing (..)
import SudokuTypes exposing (..)


autoSolveStateToString : AutoSolveState -> String
autoSolveStateToString state =
    case state of
        NotSolving ->
            "Not Solving"

        CanceledSolving ->
            "Canceled Solving"

        SolvingSingles ->
            "Solving Singles"

        SolvingPairs ->
            "Solving Pairs"

            

hasNumberRepeated : List Int -> Bool
hasNumberRepeated numbers =
    List.length numbers /= List.length (Set.toList (Set.fromList numbers))


indexToPosition : Int -> Position
indexToPosition index =
    if validIndex index then
        ( (index // 9) + 1, modBy 9 index + 1 )

    else
        ( 0, 0 )


newCellAt : Position -> Cell
newCellAt pos =
    if validPosition pos then
        { pos = pos
        , block = positionToBlock pos
        , value = Nothing
        , isVisible = False
        , guess = Nothing
        , marks = []
        }

    else
        newCellAt ( 1, 1 )


positionToBlock : Position -> Int
positionToBlock ( row, col ) =
    if validPosition ( row, col ) then
        (((row - 1) // 3) * 3) + ((col - 1) // 3) + 1

    else
        -1


positionToIndex : Position -> Int
positionToIndex ( row, col ) =
    if validPosition ( row, col ) then
        (row - 1) * 9 + (col - 1)

    else
        -1


validIndex : Int -> Bool
validIndex index =
    (index >= 0) && (index < 81)


validPosition : ( Int, Int ) -> Bool
validPosition ( row, col ) =
    (row >= 1) && (row <= 9) && (col >= 1) && (col <= 9)
