module Interactions exposing (..)

import Json.Decode as Decode
import SudokuTypes exposing (..)
import Updaters exposing (updateSelectedCell)


keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.map toKey (Decode.field "key" Decode.string)


moveSelectedCellDown : Model -> Model
moveSelectedCellDown model =
    updateSelectedCell 9 model


moveSelectedCellLeft : Model -> Model
moveSelectedCellLeft model =
    updateSelectedCell -1 model


moveSelectedCellRight : Model -> Model
moveSelectedCellRight model =
    updateSelectedCell 1 model


moveSelectedCellUp : Model -> Model
moveSelectedCellUp model =
    updateSelectedCell -9 model


toKey : String -> Msg
toKey keyValue =
    case String.uncons keyValue of
        Just ( char, "" ) ->
            CharacterKeyPressed char

        _ ->
            ControlKeyPressed keyValue
