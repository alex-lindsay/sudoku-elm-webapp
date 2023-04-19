module Sudoku exposing (..)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Array exposing (Array)


type GameState
    = GenerateBoard
    | SetKnown
    | SetGuess
    | SetMarks


type Msg =
    SetGameState GameState

type alias Cell =
    { value : Maybe Int
    , isVisible : Bool
    , guess : Maybe Int
    , marks : List Int
    }

type alias Model =
    { gameState : GameState
    , cells : Array (Array Cell)
    }

newCell : Cell
newCell =
    { value = Nothing
    , isVisible = False
    , guess = Nothing
    , marks = []
    }


newBoard : Array (Array Cell)
newBoard = Array.repeat 9 (Array.repeat 9 newCell)


init : Model
init = 
    { gameState = SetKnown
    , cells = newBoard
    }

update : Msg -> Model -> Model
update msg model =
  case msg of
    SetGameState gameState ->
        { model | gameState = gameState }

view : Model -> Html Msg
view model =
    div []
        [ button [ onClick (SetGameState GenerateBoard) ] [ text "New game" ]
        , button [ onClick (SetGameState SetKnown) ] [ text "Known" ]
        , button [ onClick (SetGameState SetGuess) ] [ text "Guess" ]
        , button [ onClick (SetGameState SetMarks) ] [ text "Marks" ]
        ]
   

main : Program () Model Msg
main =
  Browser.sandbox { init = init, update = update, view = view }

