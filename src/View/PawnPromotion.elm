module View.PawnPromotion exposing (..)

import Browser
import Chess exposing (..)
import Html exposing (Html, button, br, node, div, ul, li, span, text, input)
import Html.Attributes exposing (width, height, style, disabled, title)
import Html.Events exposing (onInput, onClick)
import Icon exposing (pieceToIcon)
import Theme exposing (
    darkSpaceColor, darkSpaceColor
  , lightSpaceColor, borderColor, whitePlayerColor
  , blackPlayerColor, checkSize
  )

itemView : (PawnPromotion -> m) -> PawnPromotion -> Html m
itemView choose pr =
  li
  [ onClick <| choose pr
  , style "font-size" checkSize
  , style "margin" "0 10px"
  ]
  [ text <| pieceToIcon <| Piece White <| promote pr ]

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
