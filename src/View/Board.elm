module View.Board exposing (..)

import Alphabet exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Theme exposing (..)

rankBorderCellView : Int -> Html a
rankBorderCellView i =
  div
  [ style "width" (intToPx (tileSize // 2))
  , style "display" "flex"
  , style "justify-content" "center"
  , style "align-items" "center"
  , style "background-color" borderColor
  ]
  [ text <| String.fromInt <| i + 1 ]


fileBorderRowView : Int -> Html a
fileBorderRowView n =
  div
  [ style "background-color" borderColor
  , style "display" "flex"
  , style "height" (intToPx (tileSize // 2))
  , style "margin" "0 auto"
  , style "padding" ("0 " ++ intToPx (tileSize // 2))
  , style "text-align" "center"
  , style "v-align" "center"
  ]
  (List.range 0 (n - 1) |> List.map
    (intToAlphabet
    >> text
    >> List.singleton
    >> div
      [ style "width" (intToPx tileSize)
      , style "display" "flex"
      , style "justify-content" "center"
      , style "align-items" "center"
      ]
    )
  )