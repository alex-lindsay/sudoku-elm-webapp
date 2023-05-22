module Sudoku exposing (..)

-- import Random exposing (int)

import SudokuTypes exposing (..)
import Array exposing (Array, initialize)
import Array.Extra exposing (..)
import Browser exposing (..)
import Browser.Events exposing (onKeyDown)
import Html exposing (Html, a, button, div, h1, text)
import Html.Attributes exposing (class, classList, hidden, href, title)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import List exposing (range)
import Process exposing (..)
import Set exposing (..)
import Task exposing (..)


validIndex : Int -> Bool
validIndex index =
    (index >= 0) && (index < 81)


validPosition : ( Int, Int ) -> Bool
validPosition ( row, col ) =
    (row >= 1) && (row <= 9) && (col >= 1) && (col <= 9)


indexToPosition : Int -> Position
indexToPosition index =
    if validIndex index then
        ( (index // 9) + 1, modBy 9 index + 1 )

    else
        ( 0, 0 )


positionToIndex : Position -> Int
positionToIndex ( row, col ) =
    if validPosition ( row, col ) then
        (row - 1) * 9 + (col - 1)

    else
        -1


positionToBlock : Position -> Int
positionToBlock ( row, col ) =
    if validPosition ( row, col ) then
        (((row - 1) // 3) * 3) + ((col - 1) // 3) + 1

    else
        -1


newCellAt : Position -> Cell
newCellAt ( row, col ) =
    if validPosition ( row, col ) then
        { row = row
        , col = col
        , block = positionToBlock ( row, col )
        , value = Nothing
        , isVisible = False
        , guess = Nothing
        , marks = []
        }

    else
        newCellAt ( 1, 1 )


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


emptyBoard : Array Cell
emptyBoard =
    initialize 81 (\i -> newCellAt (indexToPosition i))


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


cellGuessOrValue : Cell -> Maybe Int
cellGuessOrValue cell =
    if cell.guess /= Nothing then
        cell.guess

    else
        cell.value


cellGuessOrKnown : Cell -> Maybe Int
cellGuessOrKnown cell =
    case ( cell.guess, cell.isVisible ) of
        ( Just guess, _ ) ->
            Just guess

        ( Nothing, True ) ->
            cell.value

        _ ->
            Nothing


rowCells : Int -> Model -> List Cell
rowCells rowNumber model =
    List.filter (\cell -> cell.row == rowNumber) (Array.toList model.cells)


colCells : Int -> Model -> List Cell
colCells colNumber model =
    List.filter (\cell -> cell.col == colNumber) (Array.toList model.cells)


blockCells : Int -> Model -> List Cell
blockCells blockNumber model =
    List.filter (\cell -> cell.block == blockNumber) (Array.toList model.cells)


hasNumberRepeated : List Int -> Bool
hasNumberRepeated numbers =
    List.length numbers /= List.length (Set.toList (Set.fromList numbers))


cellsHaveNumberRepeated : List Cell -> (Cell -> Maybe Int) -> Bool
cellsHaveNumberRepeated cells getNumber =
    hasNumberRepeated (List.filterMap getNumber cells)


rowHasNumberRepeated : Int -> (Cell -> Maybe Int) -> Model -> Bool
rowHasNumberRepeated rowNumber getNumber model =
    cellsHaveNumberRepeated (rowCells rowNumber model) getNumber


anyRowHasValueRepeated : Model -> Bool
anyRowHasValueRepeated model =
    List.any (\rowNumber -> rowHasNumberRepeated rowNumber .value model) (range 1 9)


anyRowHasGuessRepeated : Model -> Bool
anyRowHasGuessRepeated model =
    List.any (\rowNumber -> rowHasNumberRepeated rowNumber .guess model) (range 1 9)


colHasNumberRepeated : Int -> (Cell -> Maybe Int) -> Model -> Bool
colHasNumberRepeated colNumber getNumber model =
    cellsHaveNumberRepeated (colCells colNumber model) getNumber


anyColHasValueRepeated : Model -> Bool
anyColHasValueRepeated model =
    List.any (\colNumber -> colHasNumberRepeated colNumber .value model) (range 1 9)


anyColHasGuessRepeated : Model -> Bool
anyColHasGuessRepeated model =
    List.any (\colNumber -> colHasNumberRepeated colNumber .guess model) (range 1 9)


blockHasNumberRepeated : Int -> (Cell -> Maybe Int) -> Model -> Bool
blockHasNumberRepeated blockNumber getNumber model =
    cellsHaveNumberRepeated (blockCells blockNumber model) getNumber


anyBlockHasValueRepeated : Model -> Bool
anyBlockHasValueRepeated model =
    List.any (\blockNumber -> blockHasNumberRepeated blockNumber .value model) (range 1 9)


anyBlockHasGuessRepeated : Model -> Bool
anyBlockHasGuessRepeated model =
    List.any (\blockNumber -> blockHasNumberRepeated blockNumber .guess model) (range 1 9)


cellsAreComplete : List Cell -> (Cell -> Maybe Int) -> Bool
cellsAreComplete cells getNumber =
    List.all (\cell -> getNumber cell /= Nothing) cells && not (cellsHaveNumberRepeated cells getNumber)


rowIsComplete : Int -> (Cell -> Maybe Int) -> Model -> Bool
rowIsComplete rowNumber getNumber model =
    cellsAreComplete (rowCells rowNumber model) getNumber


allRowsAreComplete : Model -> Bool
allRowsAreComplete model =
    List.all (\rowNumber -> rowIsComplete rowNumber cellGuessOrValue model) (range 1 9)


colIsComplete : Int -> (Cell -> Maybe Int) -> Model -> Bool
colIsComplete colNumber getNumber model =
    cellsAreComplete (rowCells colNumber model) getNumber


allColsAreComplete : Model -> Bool
allColsAreComplete model =
    List.all (\colNumber -> colIsComplete colNumber cellGuessOrValue model) (range 1 9)


blockIsComplete : Int -> (Cell -> Maybe Int) -> Model -> Bool
blockIsComplete blockNumber getNumber model =
    cellsAreComplete (blockCells blockNumber model) getNumber


allBlocksAreComplete : Model -> Bool
allBlocksAreComplete model =
    List.all (\blockNumber -> rowIsComplete blockNumber cellGuessOrValue model) (range 1 9)


hasWinningStatusUnknown : Model -> Bool
hasWinningStatusUnknown model =
    List.any (\cell -> ( cell.value, cell.guess ) == ( Nothing, Nothing )) (Array.toList model.cells)


hasWinningStatusWon : Model -> Bool
hasWinningStatusWon model =
    allRowsAreComplete model && allColsAreComplete model && allBlocksAreComplete model


hasWinningStatusLost : Model -> Bool
hasWinningStatusLost model =
    List.all (\cell -> cell.value /= Nothing || cell.guess /= Nothing) (Array.toList model.cells)
        && not (hasWinningStatusWon model)


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


guessesAndKnownsForCells : List Cell -> List Int
guessesAndKnownsForCells values =
    List.map cellGuessOrKnown values
        |> List.filterMap identity


guessesAndKnownsForCellAt : ( Int, Int ) -> Model -> List Int
guessesAndKnownsForCellAt ( row, col ) model =
    let
        block =
            positionToBlock ( row, col )

        rowValues =
            rowCells row model
                |> guessesAndKnownsForCells

        colValues =
            colCells col model
                |> guessesAndKnownsForCells

        blockValues =
            blockCells block model
                |> guessesAndKnownsForCells

        values =
            rowValues
                ++ colValues
                ++ blockValues
                |> Set.fromList
                |> Set.toList

        -- _ = Debug.log "row, col, block" (row, col, block)
        -- _ = Debug.log "rowGuessValues" rowValues
        -- _ = Debug.log "colGuessValues" colValues
        -- _ = Debug.log "blockGuessValues" blockValues
        -- _ = Debug.log "guessValues" values
    in
    values


autoHintsForCellAt : ( Int, Int ) -> Model -> List Int
autoHintsForCellAt ( row, col ) model =
    let
        allPossibleValues =
            Set.fromList [ 1, 2, 3, 4, 5, 6, 7, 8, 9 ]

        knownValues =
            guessesAndKnownsForCellAt ( row, col ) model
                |> Set.fromList
    in
    Set.diff allPossibleValues knownValues
        |> Set.toList


generateAutoMarks : Model -> Model
generateAutoMarks model =
    let
        -- for cells which don't have a guess, or known value, set the marks to the auto marks
        newCells =
            range 0 80
                |> List.map
                    (\i ->
                        Array.get i model.cells
                            |> Maybe.withDefault (newCellAt (indexToPosition i))
                    )
                |> List.map
                    (\cell ->
                        let
                            autoMarks =
                                autoHintsForCellAt ( cell.row, cell.col ) model
                        in
                        case ( cell.guess, cell.value, cell.isVisible ) of
                            ( Nothing, Nothing, _ ) ->
                                { cell | marks = autoMarks }

                            ( Nothing, Just _, False ) ->
                                { cell | marks = autoMarks }

                            _ ->
                                cell
                    )
                |> Array.fromList
    in
    { model | cells = newCells }


updateActiveNumber : Maybe Int -> Model -> Model
updateActiveNumber activeNumber model =
    { model | activeNumber = activeNumber }


updateAutoSolveState : AutoSolveState -> Model -> Model
updateAutoSolveState newAutoSolveState model =
    { model | autoSolveState = newAutoSolveState }


updateCellValue : Position -> Model -> Model
updateCellValue pos model =
    let
        index =
            positionToIndex pos

        cell =
            model.cells
                |> Array.get index
                |> Maybe.withDefault (newCellAt pos)

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
                            if List.member number cell.marks then
                                { cell | marks = List.filter (\mark -> mark /= number) cell.marks }

                            else
                                { cell | marks = List.append cell.marks [ number ] }

                        Nothing ->
                            cell

                Just SetAutoMarks ->
                    let
                        autoMarks =
                            autoHintsForCellAt pos model
                    in
                    if cell.marks /= autoMarks then
                        { cell | marks = autoMarks }

                    else
                        cell

                Nothing ->
                    cell
    in
    updateWinningStatus { model | cells = Array.set index updatedCell model.cells, selectedCell = pos }


updateCurrentCellValue : Model -> Model
updateCurrentCellValue model =
    updateCellValue model.selectedCell model


updateGameState : GameState -> Model -> Model
updateGameState gameState model =
    if model.gameState == Just gameState then
        { model | gameState = Maybe.Nothing }

    else
        { model | gameState = Just gameState }


updateSelectedCell : Int -> Model -> Model
updateSelectedCell delta model =
    let
        index =
            positionToIndex model.selectedCell

        newIndex =
            index
                + delta
                |> modBy 81

        ( newRow, newCol ) =
            indexToPosition newIndex
    in
    if validPosition ( newRow, newCol ) then
        { model | selectedCell = ( newRow, newCol ) }

    else
        model


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


cellsWithSingleMark : Array Cell -> Array Cell
cellsWithSingleMark cells =
    Array.filter (\cell -> List.length cell.marks == 1) cells


updateSingle : Model -> Model
updateSingle model =
    let
        firstCellWithSingleMark =
            cellsWithSingleMark model.cells
                |> Array.get 0

        newCells =
            case firstCellWithSingleMark of
                Just cell ->
                    let
                        index =
                            positionToIndex ( cell.row, cell.col )

                        updatedCell =
                            { cell | guess = List.head cell.marks, marks = [] }
                    in
                    Array.set index updatedCell model.cells

                Nothing ->
                    model.cells
    in
    { model | cells = newCells }


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
            init ()

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
            case cellsWithSingleMark model.cells |> Array.length of
                0 ->
                    ( updateAutoSolveState NotSolving model, Cmd.none )

                _ ->
                    ( updateAutoSolveState SolvingSingles model |> updateSingle |> generateAutoMarks, Process.sleep 2000 |> Task.perform (\_ -> SolveSingles) )


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
                [ h1 [] [ text "Sudoku" ]
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


keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.map toKey (Decode.field "key" Decode.string)


toKey : String -> Msg
toKey keyValue =
    case String.uncons keyValue of
        Just ( char, "" ) ->
            CharacterKeyPressed char

        _ ->
            ControlKeyPressed keyValue


subscriptions : Model -> Sub Msg
subscriptions _ =
    onKeyDown keyDecoder


main : Program () Model Msg
main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }
