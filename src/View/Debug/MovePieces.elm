module View.Debug.MovePieces exposing (..)

import Chess.Base exposing (..)
import Direction exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Result.Extra as R

moveText : PieceMove -> Html a
moveText = Debug.toString >> text 

movePiecesView : List PieceMove -> Html m
movePiecesView moves =
  div
  [ style "margin" "1em"
  , style "border" "1px solid black"
  ]
  [ moves
    |> List.indexedMap
      (\i ->
        moveText
        >> List.singleton
        >> li [ style "backgroundColor" <| if modBy 2 i == 0 then "white" else "lightgrey" ]
      )
    |> ul
      [ style "list-style" "none"
      , style "margin" "0"
      , style "min-height" "100px"
      , style "padding" "0"
      ]
  ]