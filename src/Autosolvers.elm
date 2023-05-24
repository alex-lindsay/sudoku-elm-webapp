module Autosolvers exposing (..)

import Array exposing (Array)
import Helpers exposing (positionToIndex)
import SudokuTypes exposing (Cell, Model)

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


