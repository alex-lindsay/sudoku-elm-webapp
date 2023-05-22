module Navigation exposing (..)

import SudokuTypes exposing (..)
import Updaters exposing (updateSelectedCell)

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
