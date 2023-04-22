module Sudoku exposing (..)

-- import Debug exposing (log, toString)

import Array exposing (Array)
import Browser
import Html exposing (Html, button, div, h1, text)
import Html.Attributes exposing (class, classList, disabled)
import Html.Events exposing (onClick)
import List exposing (append, range)


type GameState
    = SetKnown
    | SetGuess
    | SetMarks


type alias Position =
    ( Int, Int )


type Msg
    = SetGameState GameState
    | SetActiveNumber (Maybe Int)
    | SetCellValue ( Int, Int )
    | GenerateBoard


type alias Cell =
    { row : Int
    , col : Int
    , value : Maybe Int
    , isVisible : Bool
    , guess : Maybe Int
    , marks : List Int
    }


type alias Model =
    { gameState : GameState
    , activeNumber : Maybe Int
    , cells : Array Cell
    , selectedCell : Maybe Position
    }


indexToPosition : Int -> Position
indexToPosition index =
    ( (index // 9) + 1, modBy 9 index + 1 )


positionToIndex : Position -> Int
positionToIndex ( row, col ) =
    (row - 1) * 9 + (col - 1)


newCellAt : Position -> Cell
newCellAt ( row, col ) =
    { row = row
    , col = col
    , value = Nothing
    , isVisible = False
    , guess = Nothing
    , marks = []
    }


init : ( Model, Cmd Msg )
init =
    ( { gameState = SetKnown
      , activeNumber = Nothing
      , cells = Array.initialize 81 (\i -> newCellAt (indexToPosition i))
      , selectedCell = Nothing
      }
    , Cmd.none
    )


update : Msg -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
update msg ( model, _ ) =
    case msg of
        SetGameState gameState ->
            ( { model | gameState = gameState }, Cmd.none )

        SetActiveNumber activeNumber ->
            ( { model | activeNumber = activeNumber }, Cmd.none )

        SetCellValue ( row, col ) ->
            let
                index =
                    positionToIndex ( row, col )

                cell =
                    model.cells
                        |> Array.get index
                        |> Maybe.withDefault (newCellAt ( row, col ))

                newCell =
                    case model.gameState of
                        SetKnown ->
                            { cell | value = model.activeNumber }

                        SetGuess ->
                            { cell | guess = model.activeNumber }

                        SetMarks ->
                            case model.activeNumber of
                                Just number ->
                                    { cell | marks = append cell.marks [ number ] }

                                Nothing ->
                                    cell
            in
            ( { model | cells = Array.set index newCell model.cells }, Cmd.none )

        GenerateBoard ->
            ( model, Cmd.none )


viewCellAt : Model -> Position -> Html Msg
viewCellAt model ( row, col ) =
    let
        index =
            positionToIndex ( row, col )

        cell =
            model.cells
                |> Array.get index
                |> Maybe.withDefault (newCellAt ( row, col ))
    in
    div [ classList [ ( "cell", True ), ( "cell--selected", model.selectedCell == Just ( row, col ) ), ( "row" ++ String.fromInt row, True ), ( "col" ++ String.fromInt col, True ) ] ]
        [ case cell.value of
            Just value ->
                div [ class "cell__value" ]
                    [ text (String.fromInt value) ]

            Nothing ->
                div [] []
        , case cell.guess of
            Just guess ->
                div [ class "cell__guess" ]
                    [ text (String.fromInt guess) ]

            Nothing ->
                div [] []
        , div [ class "cell__marks" ]
            [ text (Debug.log "marks" "") ]

        -- [ text (String.fromInt cell.marks) ]
        ]


view : ( Model, Cmd Msg ) -> Html Msg
view ( model, _ ) =
    let
        _ =
            Debug.log "model" model
    in
    div [ class "sudoku-game-container" ]
        [ div [ class "sudoku-game" ]
            [ h1 [] [ text "Sudoku" ]
            , div [ class "game-state-buttons" ]
                [ button
                    [ onClick (SetGameState SetKnown)
                    , classList [ ( "active", model.gameState == SetKnown ) ]
                    ]
                    [ text "Set Known" ]
                , button
                    [ onClick (SetGameState SetGuess)
                    , classList [ ( "active", model.gameState == SetGuess ) ]
                    ]
                    [ text "Set Guess" ]
                , button
                    [ onClick (SetGameState SetMarks)
                    , classList [ ( "active", model.gameState == SetMarks ) ]
                    ]
                    [ text "Set Marks" ]
                ]
            , div [ class "number-buttons" ]
                [ button [ onClick (SetActiveNumber (Just 1)) ] [ text "1" ]
                , button [ onClick (SetActiveNumber (Just 2)) ] [ text "2" ]
                , button [ onClick (SetActiveNumber (Just 3)) ] [ text "3" ]
                , button [ onClick (SetActiveNumber (Just 4)) ] [ text "4" ]
                , button [ onClick (SetActiveNumber (Just 5)) ] [ text "5" ]
                , button [ onClick (SetActiveNumber (Just 6)) ] [ text "6" ]
                , button [ onClick (SetActiveNumber (Just 7)) ] [ text "7" ]
                , button [ onClick (SetActiveNumber (Just 8)) ] [ text "8" ]
                , button [ onClick (SetActiveNumber (Just 9)) ] [ text "9" ]
                , button [ onClick (SetActiveNumber Nothing) ] [ text "Clear" ]
                ]
            , div [ class "generator-buttons" ]
                [ button [ onClick GenerateBoard ] [ text "Generate Board" ]
                ]
            , div [ class "board-container" ]
                (List.range 0 80
                    |> List.map indexToPosition
                    |> List.map (viewCellAt model)
                )
            ]
        ]


main : Program () ( Model, Cmd Msg ) Msg
main =
    Browser.sandbox { init = init, update = update, view = view }
