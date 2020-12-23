module View.MoveHistory exposing (..)

import Chess exposing (..)
import Chess.AlgebraicNotation exposing (..)
import Component exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Result.Extra as R

moveHistoryView : Game -> Html a
moveHistoryView g =
  gameAN g
  |> R.unwrap
    blank
    (List.indexedMap
      (\i x ->
        li
        [ style "backgroundColor" <| if modBy 2 i == 0 then "white" else "lightgrey" ]
        [ text x ]
      )
    >> ul
      [ style "list-style" "none"
      , style "margin" "0"
      , style "min-height" "100px"
      , style "padding" "0"
      ]
    )
