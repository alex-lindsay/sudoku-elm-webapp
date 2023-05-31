module Updaters exposing (..)

import Array exposing (..)
import CellHelpers exposing (..)
import Helpers exposing (..)
import SudokuTypes exposing (..)
import WinningStatus exposing (..)


generateAutoMarks : Model -> Model
generateAutoMarks model =
    let
        -- for cells which don't have a guess, or known value, set the marks to the auto marks
        newCells =
            List.range 0 80
                |> List.map
                    (\i ->
                        Array.get i model.cells
                            |> Maybe.withDefault (newCellAt (indexToPosition i))
                    )
                |> List.map
                    (\cell ->
                        let
                            autoMarks =
                                autoHintsForCellAt cell.pos model
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
