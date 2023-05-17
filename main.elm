module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)


-- Model


type alias Model =
    { tiles : List Int
    , isSolved : Bool
    }


init : Model
init =
    { tiles = [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 0 ]
    , isSolved = False
    }


-- Msg


type Msg
    = MoveTile Int
    | CheckSolved


-- Update


update : Msg -> Model -> Model
update msg model =
    case msg of
        MoveTile tile ->
            let
                tiles = model.tiles
                index = List.index tile tiles
                zeroIndex = List.index 0 tiles
                newTiles =
                    if isAdjacent index zeroIndex then
                        List.map (replaceAtIndex index 0 >> replaceAtIndex zeroIndex tile) tiles
                    else
                        tiles
                isSolved = checkSolved newTiles
            in
            { model | tiles = newTiles, isSolved = isSolved }

        CheckSolved ->
            let
                isSolved = checkSolved model.tiles
            in
            { model | isSolved = isSolved }


replaceAtIndex : Int -> a -> List a -> List a
replaceAtIndex index item list =
    if index < 0 || index >= List.length list then
        list
    else
        List.Extra.updateAt index (always item) list


isAdjacent : Int -> Int -> Bool
isAdjacent index1 index2 =
    let
        row1 = index1 // 4
        col1 = index1 % 4
        row2 = index2 // 4
        col2 = index2 % 4
    in
    (row1 == row2 && abs (col1 - col2) == 1) || (col1 == col2 && abs (row1 - row2) == 1)


checkSolved : List Int -> Bool
checkSolved tiles =
    tiles == [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 0 ]


-- View


view : Model -> Html Msg
view model =
    div []
        [ div [ class "grid" ] (List.map viewTile model.tiles)
        , div [] [ text (if model.isSolved then "Puzzle solved!" else "") ]
        ]


viewTile : Int -> Html Msg
viewTile tile =
    button [ class "tile", onClick (MoveTile tile) ] [ text (toString tile) ]


main =
    Browser.sandbox { init = init, update = update, view = view }
