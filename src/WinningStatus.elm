module WinningStatus exposing (..)

import CellHelpers exposing (..)
import SudokuTypes exposing (..)
import Array exposing (..)

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

