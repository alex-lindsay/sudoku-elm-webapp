module Sudoku exposing (Model, Msg(..), main, update, view)

import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)


-- MODEL


type alias Model =
    { cells : List (List Cell)
    , activeCell : Maybe (Int, Int)
    , mode : Mode
    }


type alias Cell =
    { value : Maybe Int
    , displayValue : Bool
    , guess : Maybe Int
    , marks : List Int
    }


type Mode
    = SetKnown
    | SetGuess
    | SetMarks


initialModel : Model
initialModel =
    { cells = List.repeat 9 (List.repeat 9 emptyCell)
    , activeCell = Nothing
    , mode = SetKnown
    }


emptyCell : Cell
emptyCell =
    { value = Nothing
    , displayValue = False
    , guess = Nothing
    , marks = []
    }


-- UPDATE


type Msg
    = CellClicked (Int, Int)
    | ModeSet Mode
    | NumberClicked Int
    | ClearClicked


update : Msg -> Model -> Model
update msg model =
    case msg of
        CellClicked coords ->
            { model | activeCell = Just coords }

        ModeSet newMode ->
            { model | mode = newMode }

        NumberClicked num ->
            let
                (x, y) =
                    case model.activeCell of
                        Just coords ->
                            coords

                        Nothing ->
                            (0, 0)

                cell =
                    case model.cells !! x !! y of
                        { value = Nothing, displayValue = _, guess = _, marks = _ } ->
                            case model.mode of
                                SetKnown ->
                                    { model.cells !! x !! y | value = Just num }

                                SetGuess ->
                                    { model.cells !! x !! y | guess = Just num }

                                SetMarks ->
                                    { model.cells !! x !! y | marks = toggleMark num (model.cells !! x !! y).marks }

                        _ ->
                            model.cells !! x !! y
            in
                { model | cells = replace2D x y cell model.cells }

        ClearClicked ->
            let
                (x, y) =
                    case model.activeCell of
                        Just coords ->
                            coords

                        Nothing ->
                            (0, 0)

                cell =
                    case model.cells !! x !! y of
                        { value = _, displayValue = _, guess = _, marks = _ } ->
                            case model.mode of
                                SetKnown ->
                                    { model.cells !! x !! y | value = Nothing }

                                SetGuess ->
                                    { model.cells !! x !! y | guess = Nothing }

                                SetMarks ->
                                    { model.cells !! x !! y | marks = [] }

                        _ ->
                            model.cells !! x !! y
            in
                { model | cells = replace2D x y cell model.cells }


toggleMark : Int -> List Int -> List Int
toggleMark mark marks =
    if List.member mark marks then
        List.filter ((/=) mark) marks
    else
        mark :: marks


view : Model -> Html Msg
view model =
    let
        cellView : Int -> Int -> Html Msg
        cellView row col =
            let
                cell = model.board.(row).(col)
                isHighlighted = model.activeCell == Just (row, col)
                isGuess = case cell of
                    Guess _ -> True
                    _ -> False
                isMarked num = case cell of
                    Marks marks -> List.member num marks
                    _ -> False
            in
            td
                [ classList
                    [ ("cell", True)
                    , ("highlighted", isHighlighted)
                    , ("guess", isGuess)
                    ]
                , onClick (SelectCell (row, col))
                ]
                [ case cell of
                    Known value ->
                        text <| String.fromInt value

                    Guess value ->
                        text <| String.fromInt value

                    Marks _ ->
                        div []
                            (List.map
                                (\num ->
                                    div
                                        [ classList [("mark", True), ("marked", isMarked num)] ]
                                        [ text <| String.fromInt num ]
                                )
                                [ 1 .. 9 ]
                            )
                ]

        rowView : Int -> Html Msg
        rowView row =
            tr
                [ classList [("row", True), ("even-row", row // 3 % 2 == 0), ("odd-row", row // 3 % 2 == 1)] ]
                (List.concatMap
                    (\col -> [ cellView row col, if col == 8 then text "" else text " " ])
                    [ 0 .. 8 ]
                )
    in
    table
        [ classList [("board", True)] ]
        (List.concatMap
            (\row -> [ rowView row, if row == 8 then text "" else text " " ])
            [ 0 .. 8 ]
        )


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }
