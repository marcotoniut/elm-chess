module View.Tile exposing (..)

import Chess exposing (..)
import Component exposing (blank, emptyAttribute)
import Direction exposing (..)
import Html exposing (Html, button, br, node, div, ul, li, span, text, input)
import Html.Attributes exposing (width, height, style, disabled, title)
import Html.Events exposing (onInput, onClick)
import Icon exposing (pieceToIcon)
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

tileView : (V2 -> m) -> Board -> V2 -> TileInteraction -> Maybe Piece -> Html m
tileView select b v t mp =
  let (f, r) = v
      wcs = inCheck White b v
      bcs = inCheck Black b v
  in div
    [ style "position" "relative"
    , style "backgroundColor"
      <| if (modBy 2 (f + r) == 0) then darkSpaceColor else lightSpaceColor
    , style "width" checkSize
    , style "height" checkSize
    , onClick <| select v
    ]
    [ div
      (List.concat
        [ [ style "position" "absolute"
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
        ]
      )
      []
    , span
      [ style "position" "absolute"
      , style "top" "3px"
      , style "left" "3px"
      , style "user-select" "none"
      ]
      [ text <| showTile v ]
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
