module Sudoku exposing (..)

-- import Random exposing (int)

import Array exposing (..)
import Array.Extra exposing (..)
import Autosolvers exposing (..)
import Browser exposing (..)
import Browser.Events exposing (onKeyDown)
import Constants exposing (..)
import Helpers exposing (..)
import Html exposing (Html, a, button, div, h1, text)
import Html.Attributes exposing (class, classList, hidden, href, style, title)
import Html.Events exposing (onClick)
import List exposing (range)
import Interactions exposing (..)
import Process exposing (..)
import Random exposing (..)
import Set exposing (..)
import SudokuTypes exposing (..)
import Task exposing (..)
import Updaters exposing (..)


init : () -> ( Model, Cmd Msg )
init _ =
    ( { gameState = Just SetKnown
      , activeNumber = Just 1
      , cells = initialize 81 (\i -> newCellAt (indexToPosition i))

      --   , cells = winningBoard
      , selectedCell = ( 1, 1 )
      , winningStatus = Unknown
      , autoSolveState = NotSolving
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetGameState gameState ->
            ( updateGameState gameState model, Cmd.none )

        SetActiveNumber activeNumber ->
            ( updateActiveNumber activeNumber model, Cmd.none )

        SetCellValue ( row, col ) ->
            ( updateCellValue ( row, col ) model, Cmd.none )

        GenerateBoard ->
            ( model, Array.length Constants.sampleValueStrings
            |> Random.int 0
            |> Random.generate NewPuzzle )

        NewPuzzle index ->
            let
                newBoardString = Constants.sampleValueStrings
                    |> Array.get index
                    |> Maybe.withDefault ""
            in
            ( { model | cells = fillBoardValues newBoardString emptyBoard }, Cmd.none )

        GenerateAutoMarks ->
            ( updateWinningStatus (generateAutoMarks model), Cmd.none )

        ClearAutoMarks ->
            let
                newCells =
                    model.cells
                        |> Array.toList
                        |> List.map (\cell -> { cell | marks = [] })
                        |> Array.fromList
            in
            ( updateWinningStatus { model | cells = newCells }, Cmd.none )

        CharacterKeyPressed key ->
            let
                isNumberKey =
                    key >= '1' && key <= '9'
            in
            case key of
                'k' ->
                    ( updateGameState SetKnown model, Cmd.none )

                'a' ->
                    ( updateGameState SetAnswer model, Cmd.none )

                'g' ->
                    ( updateGameState SetGuess model, Cmd.none )

                'm' ->
                    ( updateGameState SetMarks model, Cmd.none )

                'M' ->
                    ( updateGameState SetAutoMarks model, Cmd.none )

                ' ' ->
                    ( moveSelectedCellRight model, Cmd.none )

                _ ->
                    if isNumberKey then
                        ( updateActiveNumber (String.toInt (String.fromChar key)) model
                            |> updateCurrentCellValue
                            |> moveSelectedCellRight
                            |> updateWinningStatus
                        , Cmd.none
                        )

                    else
                        ( model, Cmd.none )

        ControlKeyPressed label ->
            case label of
                "ArrowRight" ->
                    ( moveSelectedCellRight model, Cmd.none )

                "ArrowLeft" ->
                    ( moveSelectedCellLeft model, Cmd.none )

                "ArrowUp" ->
                    ( moveSelectedCellUp model, Cmd.none )

                "ArrowDown" ->
                    ( moveSelectedCellDown model, Cmd.none )

                "Backspace" ->
                    ( moveSelectedCellLeft model
                        |> updateActiveNumber Nothing
                        |> updateCurrentCellValue
                        |> updateWinningStatus
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        StartSolving ->
            ( updateAutoSolveState SolvingSingles model
                |> generateAutoMarks
                |> updateSelectedCell 0
            , Process.sleep 2000 |> Task.perform (\_ -> SolveSingles)
            )

        StopSolving ->
            ( updateAutoSolveState NotSolving model, Cmd.none )

        SolveSingles ->
            case cellsWithSingleMark model.cells |> List.length of
                0 ->
                    ( updateAutoSolveState NotSolving model, Cmd.none )
                    -- we'll want to set the active cell to the first cell and then start solving the pairs

                _ ->
                    ( updateAutoSolveState SolvingSingles model |> updateSingle |> generateAutoMarks, Process.sleep 2000 |> Task.perform (\_ -> SolveSingles) )
        SolvePairs ->
            case uncheckedModelCellsWithMarkPairs model |> List.length of
                0 ->
                    ( updateAutoSolveState NotSolving model, Cmd.none )

                _ ->
                    ( updateAutoSolveState SolvingPairs model |> updatePair |> generateAutoMarks, Process.sleep 2000 |> Task.perform (\_ -> SolvePairs) )



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
    div
        [ classList [ ( "cell", True ), ( "cell--selected", model.selectedCell == ( row, col ) ), ( "row" ++ String.fromInt row, True ), ( "col" ++ String.fromInt col, True ), ( "block" ++ String.fromInt cell.block, True ) ]
        , onClick (SetCellValue ( row, col ))
        ]
        [ case ( cell.value, cell.isVisible ) of
            ( Just value, True ) ->
                div [ class "cell__value" ]
                    [ text (String.fromInt value) ]

            ( Just _, False ) ->
                div [ class "cell__answer" ]
                    [ text " " ]

            _ ->
                div [] []
        , case cell.guess of
            Just guess ->
                div [ class "cell__guess" ]
                    [ text (String.fromInt guess) ]

            Nothing ->
                div [] []
        , case ( cell.value, cell.isVisible, cell.guess ) of
            ( Just _, False, Nothing ) ->
                div [ class "cell__marks" ]
                    (List.map (\mark -> div [ class ("mark" ++ String.fromInt mark) ] [ text (String.fromInt mark) ]) cell.marks)

            ( Nothing, _, Nothing ) ->
                div [ class "cell__marks" ]
                    (List.map (\mark -> div [ class ("mark" ++ String.fromInt mark) ] [ text (String.fromInt mark) ]) cell.marks)

            _ ->
                div [] []

        -- [ text (String.fromInt cell.marks) ]
        ]


view : Model -> Html Msg
view model =
    let
        sourceLoc =
            "https://github.com/alex-lindsay/sudoku-elm-webapp"

        -- _ =
        --     Debug.log "model.hasWinningStatusWon" (hasWinningStatusWon model)
        -- _ =
        --     Debug.log "model.rowHasNumberRepeated" (List.map (\rowNumber -> rowHasNumberRepeated rowNumber .value model) (range 1 9))
        _ =
            Debug.log "model.autoSolveState" model.autoSolveState
    in
    div []
        [ div [ class "home" ] [ a [ href "/" ] [ text "Alex Lindsay" ] ]
        , div [ class "sudoku-game-container" ]
            [ div
                [ classList
                    [ ( "sudoku-game", True )
                    , ( "status-unknown", model.winningStatus == Unknown )
                    , ( "status-won", model.winningStatus == Won )
                    , ( "status-lost", model.winningStatus == Lost )
                    , ( "status-error", model.winningStatus == Error )
                    ]
                ]
                [ h1 [] [ text "Sudoku (Work In Progress)" ]
                , div [ class "game-state-buttons" ]
                    [ button
                        [ onClick (SetGameState SetKnown)
                        , title "Set the known (visible) value for a cell. [k]"
                        , classList [ ( "active", model.gameState == Just SetKnown ) ]
                        ]
                        [ text "Set Known" ]
                    , button
                        [ onClick (SetGameState SetAnswer)
                        , title "Set the actual answer for a cell. [a]"
                        , classList [ ( "active", model.gameState == Just SetAnswer ) ]
                        ]
                        [ text "Set Answer" ]
                    , button
                        [ onClick (SetGameState SetGuess)
                        , title "Set the guess for a cell. [g]"
                        , classList [ ( "active", model.gameState == Just SetGuess ) ]
                        ]
                        [ text "Set Guess" ]
                    , button
                        [ onClick (SetGameState SetMarks)
                        , title "Set pencil marks for a cell.[m]"
                        , classList [ ( "active", model.gameState == Just SetMarks ) ]
                        ]
                        [ text "Set Marks" ]
                    , button
                        [ onClick (SetGameState SetAutoMarks)
                        , title "Set all the possible pencil marks for a cell. [M]"
                        , classList [ ( "active", model.gameState == Just SetAutoMarks ) ]
                        ]
                        [ text "Auto Marks" ]
                    ]
                , div [ class "number-buttons" ]
                    (List.append
                        (range 1 9
                            |> List.map (\number -> button [ onClick (SetActiveNumber (Just number)), classList [ ( "active", model.activeNumber == Just number ) ] ] [ text (String.fromInt number) ])
                        )
                        [ button [ onClick (SetActiveNumber Nothing) ] [ text "Clear" ] ]
                    )
                , div [ class "generator-buttons" ]
                    [ button [ onClick GenerateBoard, title "Clear the board. [!]" ] [ text "Generate Board" ]
                    , button [ onClick GenerateAutoMarks, title "Add all possible pencil marks. [!]" ] [ text "Generate Auto Marks" ]
                    , button [ onClick ClearAutoMarks, title "Clear all pencil marks. [@]" ] [ text "Clear Auto Marks" ]
                    ]
                , div [ class "solver-buttons" ]
                    [ button [ onClick StartSolving, title "Start solving the board.", hidden (model.autoSolveState /= NotSolving) ] [ text "Start Solving" ]
                    , button [ onClick StopSolving, title "Stop solving the board.", hidden (model.autoSolveState == NotSolving) ] [ text "Stop Solving" ]
                    , div [ style "display" "inline-block"
                        , style "padding" "0 0.5rem" ] [ text (autoSolveStateToString model.autoSolveState) ]
                    ]
                , div [ class "board-container" ]
                    (range 0 80
                        |> List.map indexToPosition
                        |> List.map (viewCellAt model)
                    )
                , div [ class "footnote" ]
                    [ text "Source code can be found at: "
                    , a [ href sourceLoc ] [ text sourceLoc ]
                    ]
                ]
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    onKeyDown keyDecoder


main : Program () Model Msg
main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }
