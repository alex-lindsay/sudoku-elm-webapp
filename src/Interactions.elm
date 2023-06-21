module Interactions exposing (..)

import Json.Decode as Decode
import SudokuTypes exposing (..)
import Updaters exposing (updateSelectedPos)


keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.map toKey (Decode.field "key" Decode.string)


moveSelectedPosDown : Model -> Model
moveSelectedPosDown model =
    updateSelectedPos 9 model


moveSelectedPosLeft : Model -> Model
moveSelectedPosLeft model =
    updateSelectedPos -1 model


moveSelectedPosRight : Model -> Model
moveSelectedPosRight model =
    updateSelectedPos 1 model


moveSelectedPosUp : Model -> Model
moveSelectedPosUp model =
    updateSelectedPos -9 model


toKey : String -> Msg
toKey keyValue =
    case String.uncons keyValue of
        Just ( char, "" ) ->
            CharacterKeyPressed char

        _ ->
            ControlKeyPressed keyValue
