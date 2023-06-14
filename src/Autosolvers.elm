module Autosolvers exposing (..)

import Array exposing (..)
import Helpers exposing (positionToIndex)
import SudokuTypes exposing (Cell, Model)
import Tuple exposing (first, second)


cellsWithMarkCount : Int -> Array Cell -> List Cell
cellsWithMarkCount count cells =
    cells
        |> Array.toList
        |> List.filter (\cell -> List.length cell.marks == count)


cellsWithSingleMark : Array Cell -> List Cell
cellsWithSingleMark = cellsWithMarkCount 1


cellsWithPairMarks : Array Cell -> List Cell
cellsWithPairMarks = cellsWithMarkCount 2


cellsWithThreeMarks : Array Cell -> List Cell
cellsWithThreeMarks = cellsWithMarkCount 3


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

        otherCellsWithSamePairMarks =
            case firstCellWithPairMarks of
                Just cell ->
                    cellsWithPairMarks model.cells
                        |> List.filter (\otherCell -> otherCell /= cell)
                        |> List.filter (\otherCell -> otherCell.marks == cell.marks)
                        |> List.filter (\otherCell -> (first otherCell.pos == first cell.pos) || 
                            (second otherCell.pos == second cell.pos) || 
                            (otherCell.block == cell.block))

                Nothing ->
                    []

        -- Get cells which match the row, column or block of the otherCellsWithSamePairMarks 
        -- AND the row, column or block of the otherCellsWithSamePairMarks
        -- but which AREN't any of those cells

        _ =
            Debug.log "otherCellsWithSamePairMarks" otherCellsWithSamePairMarks


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
