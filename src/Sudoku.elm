module Sudoku exposing (..)

import Array exposing (Array)
import Browser
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
-- import Debug exposing (log, toString)
import List exposing (append)


type GameState
    = SetKnown
    | SetGuess
    | SetMarks


type Msg
    = SetGameState GameState
    | SetActiveNumber (Maybe Int)
    | SetCellValue ( Int, Int )
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
    , selectedCell : Maybe ( Int, Int )
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


isGuessDisabled : Model -> Bool
isGuessDisabled model =
    case model.selectedCell of
        Just (r, c) ->
            case Array.get r model.cells of
                Just row ->
                    case Array.get c row of
                        Just cell ->
                            cell.isVisible && cell.value /= Nothing
                        Nothing ->
                            True
                Nothing ->
                    True
        Nothing ->
            False


isMarksDisabled : Model -> Bool
isMarksDisabled model =
    case model.selectedCell of
        Just (r, c) ->
            case Array.get r model.cells of
                Just row ->
                    case Array.get c row of
                        Just cell ->
                            cell.isVisible && cell.value /= Nothing
                        Nothing ->
                            True
                Nothing ->
                    True
        Nothing ->
            False

init : Model
init =
    { gameState = SetKnown
    , activeNumber = Nothing
    , cells = newBoard
    , selectedCell = Nothing
    }

updateCell : Model -> (Int, Int) -> Maybe Int -> Model
updateCell model (rowNum, colNum) value = 
    case value of
        Just _ ->
            let
                cell =
                    Array.get rowNum model.cells |> Maybe.andThen (Array.get colNum)

                -- Only update the cell value if the game state is SetKnown
                -- and there is an active number
                updatedCell =
                    case ( model.gameState, model.activeNumber, cell ) of
                        ( SetKnown, Just number, Just actualCell ) ->
                            { actualCell
                                | value =
                                    if number == 0 then
                                        Nothing

                                    else
                                        Just number
                            }

                        _ ->
                            cell |> Maybe.withDefault newCell
            in
            { model
                | cells =
                    model.cells
                        |> Array.indexedMap
                            (\r row ->
                                if r /= rowNum then
                                    row

                                else
                                    row
                                        |> Array.indexedMap
                                            (\c col ->
                                                if c /= colNum then
                                                    col

                                                else
                                                    updatedCell
                                            )
                            ),
                    selectedCell = Just (rowNum, colNum)
            }

        _ ->
            model


update : Msg -> Model -> Model
update msg model =
    case msg of
        SetGameState gameState ->
            { model | gameState = gameState }

        SetActiveNumber activeNumber ->
            { model | activeNumber = activeNumber }

        SetCellValue ( rowNum, colNum ) ->
            let
                cell =
                    Array.get rowNum model.cells |> Maybe.andThen (Array.get colNum)

                -- Only update the cell value if the game state is SetKnown
                -- and there is an active number
                updatedCell =
                    case ( model.gameState, model.activeNumber, cell ) of
                        ( SetKnown, Just number, Just actualCell ) ->
                            { actualCell
                                | value =
                                    if number == 0 then
                                        Nothing

                                    else
                                        Just number
                            }

                        _ ->
                            cell |> Maybe.withDefault newCell
            in
            { model
                | cells =
                    model.cells
                        |> Array.indexedMap
                            (\r row ->
                                if r /= rowNum then
                                    row

                                else
                                    row
                                        |> Array.indexedMap
                                            (\c col ->
                                                if c /= colNum then
                                                    col

                                                else
                                                    updatedCell
                                            )
                            ),
                    selectedCell = Just (rowNum, colNum)
            }

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
                    [ classList [ ( "active-mode", model.gameState == SetGuess ), ( "disabled", isGuessDisabled model ) ]
                    , onClick (SetGameState SetGuess)
                    ]
                    [ text "Guess" ]
                , button
                    [ classList [ ( "active-mode", model.gameState == SetMarks ), ( "disabled", isMarksDisabled model ) ]
                    , onClick (SetGameState SetMarks)
                    ]
                    [ text "Marks" ]
                ]
            , div [ class "number-buttons" ]
                (append (List.map
                    (\n ->
                        button
                            [ classList [ ( "active-number", model.activeNumber == Just n ) ]
                            , onClick (SetActiveNumber (Just n))
                            ]
                            [ text <| String.fromInt n ]
                    )
                    (List.range 1 9)
                )
                [button
                            [ classList [ ( "active-number", model.activeNumber == Just 0 ) ]
                            , onClick (SetActiveNumber (Just 0))
                            ]
                            [ text "X" ]
                ])
            ]
        , div [ class "board-container" ]
            (Array.toList
                (Array.indexedMap
                    (\rowIndex row ->
                        div [ classList [ ( "row", True ), ( "row" ++ String.fromInt rowIndex, True ) ] ]
                            (Array.toList
                                (Array.indexedMap
                                    (\colIndex col ->
                                        div [ 
                                            classList [ 
                                                ( "col", True )
                                                , ( "col" ++ String.fromInt colIndex, True )
                                                , ("active-cell", ((rowIndex, colIndex) == (model.selectedCell |> Maybe.withDefault (-1,-1)))) 
                                                ]
                                            , onClick (SetCellValue (rowIndex, colIndex)) ]
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
