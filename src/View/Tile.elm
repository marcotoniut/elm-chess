module View.Tile exposing (..)

import Alphabet exposing (intToAlphabet)
import Array
import Browser
import Chess exposing (..)
import Component exposing (blank, emptyAttribute)
import Composition exposing (standardComposition, castlingComposition)
import Debug
import Direction exposing (..)
import Html exposing (Html, button, br, node, div, ul, li, span, text, input)
import Html.Attributes exposing (width, height, style, disabled, title)
import Html.Events exposing (onInput, onClick)
import Icon exposing (pieceToIcon)
import List.Extra as L
import Matrix
import Maybe.Extra as M
import PawnPromotion as PP
import Result.Extra as R
import Theme exposing (
    darkSpaceColor, darkSpaceColor
  , lightSpaceColor, borderColor, whitePlayerColor
  , blackPlayerColor, checkSize
  )
import View.Base exposing (..)

type TileInteraction
  = TileSelected
  | TileChecked AvailableMove
  | TileCleared

tileView : (V2 -> msg) -> Board -> V2 -> TileInteraction -> Maybe Piece -> Html msg
tileView select b v t mp =
  let (i, j) = v
      wcs = inCheck White b v
      bcs = inCheck Black b v
  in div
    [ style "position" "relative"
    , style "backgroundColor"
      <| if (modBy 2 (i + j) == 0) then darkSpaceColor else lightSpaceColor
    , style "width" checkSize
    , style "height" checkSize
    , onClick <| select v
    ]
    [ div
      (List.concat
        [
          [ style "position" "absolute"
          , style "bottom" "0"
          , style "left" "0"
          , style "right" "0"
          , style "top" "0"
          ]
          , case t of
            TileSelected  ->
              [ style "backgroundColor" "mediumvioletred"
              , style "opacity" ".7"
              ]
            TileChecked m ->
              [ style "backgroundColor" "brown"
              , style "opacity" ".4"
              ]
            TileCleared   ->
              [ style "backgroundColor" "transparent"
              ]
              -- if List.isEmpty wcs
              -- then if List.isEmpty bcs then "transparent" else "blue"
              -- else if List.isEmpty bcs then "red"         else "magenta"
        ]
      )
      []
    , span
      [ style "position" "absolute"
      , style "top" "3px"
      , style "left" "3px"
      , style "user-select" "none"
      ]
      [ text <| showTile (j, i) ]
    , mp
      |> Maybe.map
        (\p ->
          span
          [ style "position" "absolute"
          , style "left" "50%"
          , style "top" "50%"
          , style "transform" "translate(-50%, -50%)"
          , style "font-size" checkSize
          , style "user-select" "none"
          ]
          [ text <| pieceToIcon p ]
        )
      |> Maybe.withDefault blank
    ]
