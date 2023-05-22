module CellHelpers exposing (..)

import Array exposing (..)
import Constants exposing (..)
import SudokuTypes exposing (..)
import Helpers exposing (..)


allBlocksAreComplete : Model -> Bool
allBlocksAreComplete model =
    List.all (\blockNumber -> rowIsComplete blockNumber cellGuessOrValue model) digits


allColsAreComplete : Model -> Bool
allColsAreComplete model =
    List.all (\colNumber -> colIsComplete colNumber cellGuessOrValue model) digits


allRowsAreComplete : Model -> Bool
allRowsAreComplete model =
    List.all (\rowNumber -> rowIsComplete rowNumber cellGuessOrValue model) digits


anyBlockHasGuessRepeated : Model -> Bool
anyBlockHasGuessRepeated model =
    List.any (\blockNumber -> blockHasNumberRepeated blockNumber .guess model) digits


anyBlockHasValueRepeated : Model -> Bool
anyBlockHasValueRepeated model =
    List.any (\blockNumber -> blockHasNumberRepeated blockNumber .value model) digits


anyColHasGuessRepeated : Model -> Bool
anyColHasGuessRepeated model =
    List.any (\colNumber -> colHasNumberRepeated colNumber .guess model) digits


anyColHasValueRepeated : Model -> Bool
anyColHasValueRepeated model =
    List.any (\colNumber -> colHasNumberRepeated colNumber .value model) digits


anyRowHasGuessRepeated : Model -> Bool
anyRowHasGuessRepeated model =
    List.any (\rowNumber -> rowHasNumberRepeated rowNumber .guess model) digits


anyRowHasValueRepeated : Model -> Bool
anyRowHasValueRepeated model =
    List.any (\rowNumber -> rowHasNumberRepeated rowNumber .value model) digits


blockCells : Int -> Model -> List Cell
blockCells blockNumber model =
    List.filter (\cell -> cell.block == blockNumber) (Array.toList model.cells)


blockHasNumberRepeated : Int -> (Cell -> Maybe Int) -> Model -> Bool
blockHasNumberRepeated blockNumber getNumber model =
    cellsHaveNumberRepeated (blockCells blockNumber model) getNumber


blockIsComplete : Int -> (Cell -> Maybe Int) -> Model -> Bool
blockIsComplete blockNumber getNumber model =
    cellsAreComplete (blockCells blockNumber model) getNumber


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


cellsAreComplete : List Cell -> (Cell -> Maybe Int) -> Bool
cellsAreComplete cells getNumber =
    List.all (\cell -> getNumber cell /= Nothing) cells && not (cellsHaveNumberRepeated cells getNumber)


cellsHaveNumberRepeated : List Cell -> (Cell -> Maybe Int) -> Bool
cellsHaveNumberRepeated cells getNumber =
    hasNumberRepeated (List.filterMap getNumber cells)


colCells : Int -> Model -> List Cell
colCells colNumber model =
    List.filter (\cell -> cell.col == colNumber) (Array.toList model.cells)


colHasNumberRepeated : Int -> (Cell -> Maybe Int) -> Model -> Bool
colHasNumberRepeated colNumber getNumber model =
    cellsHaveNumberRepeated (colCells colNumber model) getNumber


colIsComplete : Int -> (Cell -> Maybe Int) -> Model -> Bool
colIsComplete colNumber getNumber model =
    cellsAreComplete (rowCells colNumber model) getNumber


rowCells : Int -> Model -> List Cell
rowCells rowNumber model =
    List.filter (\cell -> cell.row == rowNumber) (Array.toList model.cells)


rowHasNumberRepeated : Int -> (Cell -> Maybe Int) -> Model -> Bool
rowHasNumberRepeated rowNumber getNumber model =
    cellsHaveNumberRepeated (rowCells rowNumber model) getNumber


rowIsComplete : Int -> (Cell -> Maybe Int) -> Model -> Bool
rowIsComplete rowNumber getNumber model =
    cellsAreComplete (rowCells rowNumber model) getNumber
