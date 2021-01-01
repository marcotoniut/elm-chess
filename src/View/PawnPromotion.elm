module View.PawnPromotion exposing (..)

import Browser
import Chess.Base exposing (..)
import Html exposing (Html, button, br, node, div, img, ul, li, span, text, input)
import Html.Attributes exposing (attribute, width, height, style, disabled, src, title)
import Html.Events exposing (onInput, onClick)
import Icon exposing (pieceToIcon)
import Theme exposing (..)

itemView : (PawnPromotion -> m) -> PawnPromotion -> Html m
itemView choose pr =
  li
  [ onClick <| choose pr
  , style "font-size" (intToPx tileSize)
  , style "margin" "0 10px"
  , style "width" (intToPx tileSize)
  ]
  [ img
    [ src ("/app/icons/" ++ pieceToIcon (Piece White <| promote pr))
    ] []
  ]

view : (PawnPromotion -> m) -> Html m
view choose =
  ul
  [ style "display" "flex"
  , style "backgroundColor" "lightyellow"
  , style "list-style" "none"
  , style "margin" "0"
  , style "padding" "0"
  ]
  [ itemView choose QueenPromotion
  , itemView choose RookPromotion
  , itemView choose BishopPromotion
  , itemView choose KnightPromotion
  ]
