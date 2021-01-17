module View.Tile exposing (..)

import Chess.Base exposing (..)
import Component exposing (blank, emptyAttribute)
import Direction exposing (V2)
import Html exposing (Html, button, br, node, div, ul, li, span, text, img, input)
import Html.Attributes exposing (attribute, style, disabled, src, title)
import Html.Events exposing (onInput, onClick)
import Icon exposing (pieceToIcon)
import Maybe.Extra as M
import Theme exposing (..)
import View.Base exposing (..)
import View.Common exposing (..)

type TileInteraction
  = TileSelected
  | TileChecked AvailableMove
  | TileCleared

tileView : (V2 -> m) -> Bool -> Board -> (V2, Maybe Piece) -> TileInteraction -> Html m
tileView select enabled b t i =
  let (v, mp) = t
      (f, r)  = v
  in div
    [ style "position" "relative"
    , style "backgroundColor"
      <| if (modBy 2 (f + r) == 0) then darkSpaceColor else lightSpaceColor
    , style "width"  (intToPx tileSize)
    , style "height" (intToPx tileSize)
    , if enabled then onClick <| select v else emptyAttribute
    ]
    [ div
      (List.concat
        [ [ style "position" "absolute" ]
        , expandedStyles
        , case i of
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
          img
          [ src ("/app/icons/" ++ pieceToIcon p)
          , attribute "height" "90%"
          , attribute "width" "90%"
          , style "position" "absolute"
          , style "transform" "translate(-50%, -50%)"
          , style "left" "50%"
          , style "top" "50%"
          ] []
        )
      |> Maybe.withDefault blank
    ]
  