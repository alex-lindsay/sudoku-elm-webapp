module Autosolvers exposing (..)

-- import CellHelpers exposing (areCorelated, areRelated, cellsWhichContainMarks)

import Array exposing (..)
import CellHelpers exposing (..)
import Debug exposing (..)
import Helpers exposing (..)
import SudokuTypes exposing (..)


selectedCellIsFullHouse : Model -> Bool
selectedCellIsFullHouse model =
    selectedCellIsOpen model && (
        selectedCellIsOnlyOpenCellInRow model || 
        selectedCellIsOnlyOpenCellInCol model || 
        selectedCellIsOnlyOpenCellInBlock model
        )



-- cellsWithMarkCount : Int -> Array Cell -> Array Cell
-- cellsWithMarkCount count cells =
--     cells
--         |> Array.filter (\cell -> List.length cell.marks == count)
-- cellsWithSingleMark : Array Cell -> Array Cell
-- cellsWithSingleMark = cellsWithMarkCount 1
-- cellsWithPairMarks : Array Cell -> Array Cell
-- cellsWithPairMarks = cellsWithMarkCount 2
-- cellsWithThreeMarks : Array Cell -> Array Cell
-- cellsWithThreeMarks = cellsWithMarkCount 3
-- uncheckedModelCellsWithMarkPairs : Model -> Array Cell
-- uncheckedModelCellsWithMarkPairs model =
--     cellsWithPairMarks model.cells
--         |> Array.filter (\cell -> positionToIndex cell.pos >= positionToIndex model.selectedPos)
-- updateSingle : Model -> Model
-- updateSingle model =
--     let
--         firstCellWithSingleMark =
--             cellsWithSingleMark model.cells
--                 |> Array.get 0
--         newCells =
--             case firstCellWithSingleMark of
--                 Just cell ->
--                     let
--                         index =
--                             positionToIndex cell.pos
--                         updatedCell =
--                             { cell | guess = List.head cell.marks, marks = [] }
--                     in
--                     Array.set index updatedCell model.cells
--                 Nothing ->
--                     model.cells
--     in
--     { model | cells = newCells }
-- updatePair : Model -> Model
-- updatePair model =
--     let
--         firstCellWithPairMarks =
--             uncheckedModelCellsWithMarkPairs model
--             |> Array.get 0
--         otherCellWithSamePairMarks =
--             firstCellWithPairMarks
--                 |> Maybe.andThen (\cell -> model.cells
--                         |> Array.filter (\otherCell -> otherCell.marks == cell.marks)
--                         |> Array.filter (\otherCell -> areRelated cell otherCell)
--                         |> Array.get 0)
--         -- Get cells which match the row, column or block of the otherCellsWithSamePairMarks
--         -- AND the row, column or block of the otherCellsWithSamePairMarks
--         -- but which AREN't any of those cells
--         -- and subtract the marks found in the firstCellWithPairMarks from the marks of those cells
--         cellsToBeAdjusted =
--             case (firstCellWithPairMarks, otherCellWithSamePairMarks) of
--                 (Nothing, _) ->
--                     Array.fromList []
--                 (Just _, Nothing) ->
--                     Array.fromList []
--                 (Just cell, Just otherCell) ->
--                     model.cells
--                     |>  cellsWhichContainMarks cell.marks
--                         |> Array.filter (\thirdCell -> areCorelated cell otherCell thirdCell)
--         locsToAdjust =
--             cellsToBeAdjusted
--                 |> Array.map (\cell -> cell.pos)
--                 |> Array.toList
--         _ =
--             Debug.log "otherCellsWithSamePairMarks" otherCellWithSamePairMarks
--         _ =
--             Debug.log "cellsToBeAdjusted" cellsToBeAdjusted
--         _ =
--             Debug.log "locsToAdjust" locsToAdjust
--         newCells =
--             case firstCellWithPairMarks of
--                 Just rootCell ->
--                     model.cells
--                     |> Array.map (\cell ->
--                         if List.member cell.pos locsToAdjust then
--                             { cell | marks = List.filter (\mark -> not (List.member mark rootCell.marks)) cell.marks }
--                         else
--                             cell
--                     )
--                 Nothing ->
--                     model.cells
--     in
--     { model | cells = newCells }
