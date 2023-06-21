module Constants exposing (..)

import Array exposing (..)
import Array.Extra exposing (..)
import Helpers exposing (..)
import SudokuTypes exposing (..)


almostWinningString : String
almostWinningString =
    "123978564456312897789645231312897456645231789978564123231789645564123978897456310"


fillBoardValues : String -> Array Cell -> Array Cell
fillBoardValues valueString cells =
    let
        values =
            valueString
                |> String.split ""
                |> List.map (\s -> String.toInt s |> Maybe.withDefault 0)
                |> Array.fromList
    in
    Array.Extra.map2
        (\cell v ->
            { cell
                | isVisible = True, value =
                    case v of
                        0 ->
                            Nothing

                        n ->
                            Just n
            }
        )
        cells
        values




sampleValueStrings : Array String
sampleValueStrings =
    Array.fromList [ 
        "000000000000000000000000000000000000000000000000000000000000000000000000000000000", -- emptyBoard
        "020100000006000000503000000030000000010020600000600000800000000000000000900000000", -- openDoubles
        "020038190050070086087910200500700620000005018643000900009401030174800000005690801",
        "060708030000001000004930870030000180200000009007500000070000320000000040800006000",
        "060000104000200000000000050019000006020940000080705000203400069800320000000000700"
    ]


almostWinningBoard : Array Cell
almostWinningBoard =
    fillBoardValues almostWinningString emptyBoard


emptyBoard : Array Cell
emptyBoard =
    Array.initialize 81 (\i -> newCellAt (indexToPosition i))


digits : List Int
digits = [ 1, 2, 3, 4, 5, 6, 7, 8, 9 ]


defaultModel : Model
defaultModel = { gameState = Just SetKnown
      , activeNumber = Just 1
      , cells = initialize 81 (\i -> newCellAt (indexToPosition i))
      , selectedPos = ( 1, 1 )
      , winningStatus = Unknown
      , autoSolveState = NotSolving
      }