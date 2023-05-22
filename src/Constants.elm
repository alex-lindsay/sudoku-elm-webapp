module Constants exposing (..)

import Array exposing (..)
import Array.Extra exposing (..)
import Helpers exposing (..)
import SudokuTypes exposing (..)


almostWinningBoard : Array Cell
almostWinningBoard =
    let
        values =
            "123978564456312897789645231312897456645231789978564123231789645564123978897456310"
                |> String.split ""
                |> List.map (\s -> String.toInt s |> Maybe.withDefault 0)
                |> Array.fromList
    in
    Array.Extra.map2
        (\cell v ->
            { cell
                | value =
                    case v of
                        0 ->
                            Nothing

                        n ->
                            Just n
            }
        )
        emptyBoard
        values


emptyBoard : Array Cell
emptyBoard =
    Array.initialize 81 (\i -> newCellAt (indexToPosition i))


digits = [ 1, 2, 3, 4, 5, 6, 7, 8, 9 ]