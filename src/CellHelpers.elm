module CellHelpers exposing (..)

import Array exposing (..)
import Constants exposing (..)
import Helpers exposing (..)
import Set exposing (..)
import SudokuTypes exposing (..)


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


areCorelated : Cell -> Cell -> Cell -> Bool
areCorelated cell1 cell2 cell3 =
    cell1 /= cell2
        && cell1 /= cell3
        && ((inSameRow cell1 cell2 && inSameRow cell1 cell3)
            || (inSameCol cell1 cell2 && inSameCol cell1 cell3)
            || (inSameBlock cell1 cell2 && inSameBlock cell1 cell3))


areRelated : Cell -> Cell -> Bool
areRelated cell1 cell2 =
    cell1 /= cell2
        && (inSameRow cell1 cell2
            || inSameCol cell1 cell2
            || inSameBlock cell1 cell2)



autoHintsForCellAt : ( Int, Int ) -> Model -> List Int
autoHintsForCellAt ( row, col ) model =
    let
        allPossibleValues =
            Set.fromList digits

        knownValues =
            guessesAndKnownsForCellAt ( row, col ) model
                |> Set.fromList
    in
    Set.diff allPossibleValues knownValues
        |> Set.toList


blockCells : Int -> Model -> Array Cell
blockCells blockNumber model =
    Array.filter (\cell -> cell.block == blockNumber) model.cells


blockHasNumberRepeated : Int -> (Cell -> Maybe Int) -> Model -> Bool
blockHasNumberRepeated blockNumber getNumber model =
    cellsHaveNumberRepeated (blockCells blockNumber model) getNumber


blockIsComplete : Int -> (Cell -> Maybe Int) -> Model -> Bool
blockIsComplete blockNumber getNumber model =
    cellsAreComplete (blockCells blockNumber model) getNumber


cellCol  : Cell -> Int
cellCol cell =
    Tuple.second cell.pos


cellContainsMarks : List Int -> Cell -> Bool
cellContainsMarks marks cell =
    List.all (\mark -> List.member mark cell.marks) marks


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


cellIsInColumn : Int -> Cell -> Bool
cellIsInColumn colNumber cell =
    (cellCol cell) == colNumber


cellIsInRow : Int -> Cell -> Bool
cellIsInRow colNumber cell =
    (Tuple.first cell.pos) == colNumber


cellIsOnlyOpenCellInCells : Cell -> Array Cell -> Bool
cellIsOnlyOpenCellInCells cell cells =
    cells
        |> Array.filter (\otherCell -> otherCell /= cell)
        |> Array.toList
        |> List.all (\otherCell -> not (cellIsOpen otherCell)) 


cellIsOpen : Cell -> Bool
cellIsOpen cell =
    cell.value == Nothing && cell.guess == Nothing


cellRow  : Cell -> Int
cellRow cell =
    Tuple.first cell.pos


cellsAreComplete : Array Cell -> (Cell -> Maybe Int) -> Bool
cellsAreComplete cells getNumber =
    List.all (\cell -> getNumber cell /= Nothing) (Array.toList cells) && 
        not (cellsHaveNumberRepeated cells getNumber)


cellsHaveNumberRepeated : Array Cell -> (Cell -> Maybe Int) -> Bool
cellsHaveNumberRepeated cells getNumber =
    cells
        |> Array.toList
        |> List.filterMap getNumber
        |> hasNumberRepeated


cellsWhichContainMarks : List Int -> Array Cell -> Array Cell
cellsWhichContainMarks marks cells =
    cells
        |> Array.filter (cellContainsMarks marks)


colCells : Int -> Model -> Array Cell
colCells colNumber model =
    model.cells
    |> Array.filter (\cell -> cellIsInColumn colNumber cell)


colHasNumberRepeated : Int -> (Cell -> Maybe Int) -> Model -> Bool
colHasNumberRepeated colNumber getNumber model =
    cellsHaveNumberRepeated (colCells colNumber model) getNumber


colIsComplete : Int -> (Cell -> Maybe Int) -> Model -> Bool
colIsComplete colNumber getNumber model =
    cellsAreComplete (rowCells colNumber model) getNumber


guessesAndKnownsForCells : Array Cell -> List Int
guessesAndKnownsForCells values =
    Array.map cellGuessOrKnown values
        |> Array.toList
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


inSameBlock : Cell -> Cell -> Bool
inSameBlock cell1 cell2 =
    cell1.block == cell2.block


inSameCol : Cell -> Cell -> Bool
inSameCol cell1 cell2 =
    (Tuple.second cell1.pos) == (Tuple.second cell2.pos)


inSameRow : Cell -> Cell -> Bool
inSameRow cell1 cell2 =
    (Tuple.first cell1.pos) == (Tuple.first cell2.pos)


rowCells : Int -> Model -> Array Cell
rowCells rowNumber model =
    model.cells
    |> Array.filter (\cell -> cellIsInRow rowNumber cell)


rowHasNumberRepeated : Int -> (Cell -> Maybe Int) -> Model -> Bool
rowHasNumberRepeated rowNumber getNumber model =
    cellsHaveNumberRepeated (rowCells rowNumber model) getNumber


rowIsComplete : Int -> (Cell -> Maybe Int) -> Model -> Bool
rowIsComplete rowNumber getNumber model =
    cellsAreComplete (rowCells rowNumber model) getNumber


selectedCell : Model -> Maybe Cell
selectedCell model =
    model.cells
        |> Array.filter (\cell -> cell.pos == model.selectedPos)
        |> Array.get 0


selectedCellIsOpen : Model -> Bool
selectedCellIsOpen model =
    case selectedCell model of
        Just cell ->
            cellIsOpen cell

        Nothing ->
            False


selectedCellIsOnlyOpenCellInBlock : Model -> Bool
selectedCellIsOnlyOpenCellInBlock model =
    case selectedCell model of
        Just cell ->
            cellIsOnlyOpenCellInCells cell (rowCells (.block cell) model)

        Nothing ->
            False
            
            
selectedCellIsOnlyOpenCellInCol : Model -> Bool
selectedCellIsOnlyOpenCellInCol model =
    case selectedCell model of
        Just cell ->
            cellIsOnlyOpenCellInCells cell (rowCells (cellCol cell) model)

        Nothing ->
            False


selectedCellIsOnlyOpenCellInRow : Model -> Bool
selectedCellIsOnlyOpenCellInRow model =
    case selectedCell model of
        Just cell ->
            cellIsOnlyOpenCellInCells cell (rowCells (cellRow cell) model)

        Nothing ->
            False


selectedBlock : Model -> Array Cell
selectedBlock model =
    blockCells (positionToBlock model.selectedPos) model


selectedCol : Model -> Array Cell
selectedCol model =
    colCells (Tuple.second model.selectedPos) model


selectedBlockValues : Model -> List Int
selectedBlockValues model =
    selectedBlock model
        |> Array.toList
        |> List.filterMap cellGuessOrKnown
        

selectedColValues : Model -> List Int
selectedColValues model =
    selectedCol model
        |> Array.toList
        |> List.filterMap cellGuessOrKnown
        

selectedRow : Model -> Array Cell
selectedRow model =
    rowCells (Tuple.first model.selectedPos) model


selectedRowValues : Model -> List Int
selectedRowValues model =
    selectedRow model
        |> Array.toList
        |> List.filterMap cellGuessOrKnown