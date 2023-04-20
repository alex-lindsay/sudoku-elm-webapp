module Sudoku exposing (..)

import Array exposing (Array)
import Browser
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)


type GameState
    = SetKnown
    | SetGuess
    | SetMarks


type Msg
    = SetGameState GameState
    | SetActiveNumber (Maybe Int)
    | GenerateBoard


type alias Cell =
    { value : Maybe Int
    , isVisible : Bool
    , guess : Maybe Int
    , marks : List Int
    }


type alias Model =
    { gameState : GameState
    , activeNumber : Maybe Int
    , cells : Array (Array Cell)
    , selectedCell : Maybe (Int, Int)
    }


newCell : Cell
newCell =
    { value = Nothing
    , isVisible = False
    , guess = Nothing
    , marks = []
    }


newBoard : Array (Array Cell)
newBoard =
    Array.repeat 9 (Array.repeat 9 newCell)


init : Model
init =
    { gameState = SetKnown
    , activeNumber = Nothing
    , cells = newBoard
    , selectedCell = Nothing
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        SetGameState gameState ->
            { model | gameState = gameState }

        SetActiveNumber activeNumber ->
            { model | activeNumber = activeNumber }

        GenerateBoard ->
            init


view : Model -> Html Msg
view model =
    div []
        [ div [ class "game-controls" ]
            [ div [ class "game-mode" ]
                [ button [ onClick GenerateBoard ] [ text "New game" ]
                , button
                    [ classList [ ( "active-mode", model.gameState == SetKnown ) ]
                    , onClick (SetGameState SetKnown)
                    ]
                    [ text "Known" ]
                , button
                    [ classList [ ( "active-mode", model.gameState == SetGuess ) ]
                    , onClick (SetGameState SetGuess)
                    ]
                    [ text "Guess" ]
                , button
                    [ classList [ ( "active-mode", model.gameState == SetMarks ) ]
                    , onClick (SetGameState SetMarks)
                    ]
                    [ text "Marks" ]
                ]
            , div [ class "number-buttons" ]
                (List.map
                    (\n ->
                        button
                            [ classList [ ( "active-number", model.activeNumber == Just n ) ]
                            , onClick (SetActiveNumber (Just n))
                            ]
                            [ text <| String.fromInt n ]
                    )
                    (List.range 1 9)
                )
            ]
        , div [ class "board-container" ]
            (Array.toList
                (Array.indexedMap
                    (\rowIndex row ->
                        div [ classList [ ( "row", True ), ( "row" ++ String.fromInt rowIndex, True ) ] ]
                            (Array.toList
                                (Array.indexedMap
                                    (\colIndex col ->
                                        div [ classList [ ( "col", True ), ( "col" ++ String.fromInt colIndex, True ) ] ]
                                            [ div [ class "value" ]
                                                [ if Maybe.withDefault 0 col.value /= 0 then
                                                    text <| String.fromInt (Maybe.withDefault 0 col.value)

                                                  else
                                                    text ""
                                                ]
                                            ]
                                    )
                                    row
                                )
                            )
                    )
                    model.cells
                )
            )
        ]


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }
