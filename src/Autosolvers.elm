module Autosolvers exposing (..)

import Array exposing (..)
import Helpers exposing (positionToIndex)
import SudokuTypes exposing (Cell, Model)


cellsWithSingleMark : Array Cell -> List Cell
cellsWithSingleMark cells =
    cells
        |> Array.toList
        |> List.filter (\cell -> List.length cell.marks == 1)


cellsWithPairMarks : Array Cell -> List Cell
cellsWithPairMarks cells =
    cells
        |> Array.toList
        |> List.filter (\cell -> List.length cell.marks == 2)


uncheckedModelCellsWithMarkPairs : Model -> List Cell
uncheckedModelCellsWithMarkPairs model =
    cellsWithPairMarks model.cells
        |> List.filter (\cell -> positionToIndex cell.pos >= positionToIndex model.selectedCell)


updateSingle : Model -> Model
updateSingle model =
    let
        firstCellWithSingleMark =
            cellsWithSingleMark model.cells
                |> List.head

        newCells =
            case firstCellWithSingleMark of
                Just cell ->
                    let
                        index =
                            positionToIndex cell.pos

                        updatedCell =
                            { cell | guess = List.head cell.marks, marks = [] }
                    in
                    Array.set index updatedCell model.cells

                Nothing ->
                    model.cells
    in
    { model | cells = newCells }


updatePair : Model -> Model
updatePair model =
    let
        firstCellWithPairMarks =
            uncheckedModelCellsWithMarkPairs model 
            |> List.head

        newCells =
            case firstCellWithPairMarks of
                Just cell ->
                    let
                        index =
                            positionToIndex cell.pos

                        updatedCell =
                            { cell | marks = [] }
                    in
                    Array.set index updatedCell model.cells

                Nothing ->
                    model.cells
    in
    { model | cells = newCells }
