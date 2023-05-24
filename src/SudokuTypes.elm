module SudokuTypes exposing (..)

import Array exposing (Array)


type GameState
    = SetKnown
    | SetAnswer
    | SetGuess
    | SetMarks
    | SetAutoMarks


type WinningStatus
    = Won
    | Lost
    | Unknown
    | Error


type AutoSolveState
    = NotSolving
    | SolvingSingles


type alias Position =
    ( Int, Int )


type Msg
    = SetGameState GameState
    | SetActiveNumber (Maybe Int)
    | SetCellValue ( Int, Int )
    | GenerateBoard
    | GenerateAutoMarks
    | ClearAutoMarks
    | CharacterKeyPressed Char
    | ControlKeyPressed String
    | StartSolving
    | StopSolving
    | SolveSingles


type alias Cell =
    { row : Int
    , col : Int
    , block : Int
    , value : Maybe Int
    , isVisible : Bool
    , guess : Maybe Int
    , marks : List Int
    }


type alias Model =
    { gameState : Maybe GameState
    , activeNumber : Maybe Int
    , cells : Array Cell
    , selectedCell : Position
    , winningStatus : WinningStatus
    , autoSolveState : AutoSolveState
    }