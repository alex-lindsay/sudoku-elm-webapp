module Sudoku exposing (..)

import Array exposing (Array, initialize, toList)
import Browser
import Html exposing (Html, button, div, h1, text)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import List exposing (any, append, filter, filterMap, length, map, member, range)
-- import Random exposing (int)
import Set


type GameState
    = SetAnswer
    | SetKnown
    | SetGuess
    | SetMarks


type WinningStatus
    = Won
    | Lost
    | Unknown
    | Error


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
    , block : Int
    , value : Maybe Int
    , isVisible : Bool
    , guess : Maybe Int
    , marks : List Int
    }


type alias Model =
    { gameState : Maybe GameState
    , activeNumber : Maybe Int
    , cells : Array Cell
    , selectedCell : Maybe Position
    , winningStatus : WinningStatus
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
    , block = (((row - 1) // 3) * 3) + ((col - 1) // 3) + 1
    , value = Nothing
    , isVisible = False
    , guess = Nothing
    , marks = []
    }


init : ( Model, Cmd Msg )
init =
    ( { gameState = Just SetAnswer
      , activeNumber = Just 1
      , cells = initialize 81 (\i -> newCellAt (indexToPosition i))
      , selectedCell = Nothing
      , winningStatus = Unknown
      }
    , Cmd.none
    )


cellValue : Cell -> Maybe Int
cellValue cell = cell.value

cellGuess : Cell -> Maybe Int
cellGuess cell = cell.guess


rowCells : Int -> Model -> List Cell
rowCells rowNumber model =
    filter (\cell -> cell.row == rowNumber) (toList model.cells)


colCells : Int -> Model -> List Cell
colCells colNumber model =
    filter (\cell -> cell.col == colNumber) (toList model.cells)


blockCells : Int -> Model -> List Cell
blockCells blockNumber model =
    filter (\cell -> cell.block == blockNumber) (toList model.cells)


hasNumberRepeated : List Int -> Bool
hasNumberRepeated numbers =
    length numbers /= length (Set.toList (Set.fromList numbers))


cellsHaveNumberRepeated : List Cell -> (Cell -> Maybe Int) -> Bool
cellsHaveNumberRepeated cells getNumber=
    hasNumberRepeated (filterMap getNumber cells)


rowHasNumberRepeated : Int -> (Cell -> Maybe Int) -> Model -> Bool
rowHasNumberRepeated rowNumber getNumber model =
    cellsHaveNumberRepeated (rowCells rowNumber model) getNumber


anyRowHasValueRepeated : Model -> Bool
anyRowHasValueRepeated model =
    any (\rowNumber -> rowHasNumberRepeated rowNumber cellValue model) (range 1 9)


anyRowHasGuessRepeated : Model -> Bool
anyRowHasGuessRepeated model =
    any (\rowNumber -> rowHasNumberRepeated rowNumber cellGuess model) (range 1 9)


colHasNumberRepeated : Int -> (Cell -> Maybe Int) -> Model -> Bool
colHasNumberRepeated colNumber getNumber model =
    cellsHaveNumberRepeated (colCells colNumber model) getNumber


anyColHasValueRepeated : Model -> Bool
anyColHasValueRepeated model =
    any (\colNumber -> colHasNumberRepeated colNumber cellValue model) (range 1 9)


anyColHasGuessRepeated : Model -> Bool
anyColHasGuessRepeated model =
    any (\colNumber -> colHasNumberRepeated colNumber cellGuess model) (range 1 9)


blockHasNumberRepeated : Int -> (Cell -> Maybe Int) -> Model -> Bool
blockHasNumberRepeated blockNumber getNumber model =
    cellsHaveNumberRepeated (blockCells blockNumber model) getNumber


anyBlockHasValueRepeated : Model -> Bool
anyBlockHasValueRepeated model =
    any (\blockNumber -> blockHasNumberRepeated blockNumber cellValue model) (range 1 9)


anyBlockHasGuessRepeated : Model -> Bool
anyBlockHasGuessRepeated model =
    any (\blockNumber -> blockHasNumberRepeated blockNumber cellGuess model) (range 1 9)


hasWinningStatusUnknown : Model -> Bool
hasWinningStatusUnknown model =
    any (\cell -> ( cell.value, cell.guess ) == ( Nothing, Nothing )) (toList model.cells)


hasWinningStatusWon : Model -> Bool
hasWinningStatusWon model =
    False


hasWinningStatusLost : Model -> Bool
hasWinningStatusLost model =
    False
    -- any (\cell -> cell.value /= cell.guess) (toList model.cells)


hasWinningStatusError : Model -> Bool
hasWinningStatusError model =
    anyRowHasValueRepeated model || anyColHasValueRepeated model || anyBlockHasValueRepeated model || anyRowHasGuessRepeated model || anyColHasGuessRepeated model || anyBlockHasGuessRepeated model


updateWinningStatus : Model -> Model
updateWinningStatus model =
    let
        statuses =
            [ hasWinningStatusWon model, hasWinningStatusLost model, hasWinningStatusError model ]

        newWinningStatus =
            case statuses of
                [ True, _, _ ] ->
                    Won

                [ _, True, _ ] ->
                    Lost

                [ _, _, True ] ->
                    Error

                _ ->
                    Unknown
    in
    { model | winningStatus = newWinningStatus }


update : Msg -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
update msg ( model, _ ) =
    case msg of
        SetGameState gameState ->
            if model.gameState == Just gameState then
                ( { model | gameState = Maybe.Nothing }, Cmd.none )

            else
                ( { model | gameState = Just gameState }, Cmd.none )

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

                updatedCell =
                    case model.gameState of
                        Just SetAnswer ->
                            if cell.value /= model.activeNumber then
                                { cell | value = model.activeNumber, isVisible = False }

                            else
                                { cell | value = Nothing }

                        Just SetKnown ->
                            if cell.value /= model.activeNumber then
                                { cell | value = model.activeNumber, isVisible = True }

                            else
                                { cell | value = Nothing }

                        Just SetGuess ->
                            case ( cell.value, cell.isVisible ) of
                                -- Don't allow the guess if there's a visible known value for the cell
                                ( Just _, True ) ->
                                    cell

                                _ ->
                                    if cell.guess /= model.activeNumber then
                                        { cell | guess = model.activeNumber }

                                    else
                                        { cell | guess = Nothing }

                        Just SetMarks ->
                            case model.activeNumber of
                                Just number ->
                                    if member number cell.marks then
                                        { cell | marks = filter (\mark -> mark /= number) cell.marks }

                                    else
                                        { cell | marks = append cell.marks [ number ] }

                                Nothing ->
                                    cell

                        Nothing ->
                            cell
            in
            ( updateWinningStatus { model | cells = Array.set index updatedCell model.cells, selectedCell = Just ( row, col ) }, Cmd.none )

        GenerateBoard ->
            init


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
        [ classList [ ( "cell", True ), ( "cell--selected", model.selectedCell == Just ( row, col ) ), ( "row" ++ String.fromInt row, True ), ( "col" ++ String.fromInt col, True ) , ( "block" ++ String.fromInt cell.block, True ) ]
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
        , div [ class "cell__marks" ]
            (map (\mark -> div [ class ("mark" ++ String.fromInt mark) ] [ text (String.fromInt mark) ]) cell.marks)

        -- [ text (String.fromInt cell.marks) ]
        ]


view : ( Model, Cmd Msg ) -> Html Msg
view ( model, _ ) =
    let
        _ =
            Debug.log "model.anyRowHasValueRepeated" (anyRowHasValueRepeated model)
        _ =
            Debug.log "model.rowHasNumberRepeated" (List.map (\rowNumber -> rowHasNumberRepeated rowNumber cellValue model) (range 1 9))
        _ =
            Debug.log "model.winningStatus" model.winningStatus
    in
    div [ class "sudoku-game-container" ]
        [ div
            [ classList
                [ ( "sudoku-game", True )
                , ( "status-unknown", model.winningStatus == Unknown )
                , ( "status-won", model.winningStatus == Won )
                , ( "status-lost", model.winningStatus == Lost )
                , ( "status-error", model.winningStatus == Error )
                ]
            ]
            [ h1 [] [ text "Sudoku" ]
            , div [ class "game-state-buttons" ]
                [ button
                    [ onClick (SetGameState SetAnswer)
                    , classList [ ( "active", model.gameState == Just SetAnswer ) ]
                    ]
                    [ text "Set Answer" ]
                , button
                    [ onClick (SetGameState SetKnown)
                    , classList [ ( "active", model.gameState == Just SetKnown ) ]
                    ]
                    [ text "Set Known" ]
                , button
                    [ onClick (SetGameState SetGuess)
                    , classList [ ( "active", model.gameState == Just SetGuess ) ]
                    ]
                    [ text "Set Guess" ]
                , button
                    [ onClick (SetGameState SetMarks)
                    , classList [ ( "active", model.gameState == Just SetMarks ) ]
                    ]
                    [ text "Set Marks" ]
                ]
            , div [ class "number-buttons" ]
                (List.append
                    (range 1 9
                        |> List.map (\number -> button [ onClick (SetActiveNumber (Just number)), classList [ ( "active", model.activeNumber == Just number ) ] ] [ text (String.fromInt number) ])
                    )
                    [ button [ onClick (SetActiveNumber Nothing) ] [ text "Clear" ] ]
                )
            , div [ class "generator-buttons" ]
                [ button [ onClick GenerateBoard ] [ text "Generate Board" ]
                ]
            , div [ class "board-container" ]
                (range 0 80
                    |> List.map indexToPosition
                    |> List.map (viewCellAt model)
                )
            ]
        ]


main : Program () ( Model, Cmd Msg ) Msg
main =
    Browser.sandbox { init = init, update = update, view = view }
