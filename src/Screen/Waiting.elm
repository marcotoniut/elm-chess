module Screen.Waiting exposing (..)

import Chess.Base exposing (Player)
import Direction exposing (V2)
import Html exposing (..)
import Html.Attributes exposing (..)
import Matrix
import Model exposing (..)
import Theme exposing (..)
import View.Board exposing (..)
import View.Game exposing (..)

tileView : V2 -> Html m
tileView (f, r) =
  div
  [ style "position" "relative"
  , style "backgroundColor"
    <| if (modBy 2 (f + r) == 0) then darkSpaceColor else lightSpaceColor
  , style "width"  (intToPx tileSize)
  , style "height" (intToPx tileSize)
  ]
  [ div
    [ style "position" "absolute" ]
    []
  ]

waitingView : Player -> GamePlayer -> Html a
waitingView pl gpl =
  Matrix.repeat (8, 8) ()
  |> Matrix.toList
  |> List.indexedMap
    (\r xs -> div
      [ style "margin" "0 auto"
      , style "display" "flex"
      ]
      (let ranksEl = [ rankBorderCellView r ]
      in List.concat
        [ ranksEl
        , List.indexedMap
          (\f _ -> tileView (f, r))
          xs
        , ranksEl
        ]
      )
    )
    |> List.reverse
    |> div
      [ style "position" "relative"
      , style "border" "none"
      ]
