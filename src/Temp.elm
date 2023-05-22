module Temp exposing (..)

import CellHelpers exposing (..)
import Helpers exposing (..)
import SudokuTypes exposing (..)
import Set exposing (..)

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

