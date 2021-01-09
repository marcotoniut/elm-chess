module View.Common exposing (..)

import Html
import Html.Attributes exposing (style)

expandedStyles : List (Html.Attribute a)
expandedStyles =
  [ style "bottom" "0"
  , style "left"   "0"
  , style "right"  "0"
  , style "top"    "0"
  ]
