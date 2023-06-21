module Interactions exposing (..)

import Json.Decode as Decode
import SudokuTypes exposing (..)
import Updaters exposing (updateselectedPos)


keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.map toKey (Decode.field "key" Decode.string)


moveselectedPosDown : Model -> Model
moveselectedPosDown model =
    updateselectedPos 9 model


moveselectedPosLeft : Model -> Model
moveselectedPosLeft model =
    updateselectedPos -1 model


moveselectedPosRight : Model -> Model
moveselectedPosRight model =
    updateselectedPos 1 model


moveselectedPosUp : Model -> Model
moveselectedPosUp model =
    updateselectedPos -9 model


toKey : String -> Msg
toKey keyValue =
    case String.uncons keyValue of
        Just ( char, "" ) ->
            CharacterKeyPressed char

        _ ->
            ControlKeyPressed keyValue
