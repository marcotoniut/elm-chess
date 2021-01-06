module View.Board exposing (..)

import Alphabet exposing (..)
import Chess.Base exposing (..)
import Component exposing (emptyAttribute)
import Direction exposing (V2)
import Html exposing (..)
import Html.Attributes exposing (..)
import List.Extra as L
import Matrix
import Maybe.Extra as M
import Result.Extra as R
import Theme exposing (..)
import View.Base exposing (AvailableMove, ChoosingPromotion)
import View.Tile exposing (..)

type BoardAction
  = SelectTile (V2, Maybe Piece)

type alias ModelBoard =
  { board : Board
  , maybeSelected : Maybe (V2, List (V2, AvailableMove))
  , choosingPromotion : Maybe ChoosingPromotion
  }

viewBoard : (BoardAction -> a) -> ModelBoard -> Html a
viewBoard act m =
  let cp = M.isJust m.choosingPromotion
  in m.board
  |> Matrix.toList
  |> List.indexedMap
    (\r xs -> div
      [ style "margin" "0 auto"
      , style "display" "flex"
      ]
      (List.concat
        [ [ rankBorderCellView r ]
        , List.indexedMap
          (\f mp ->
            let v = (f, r)
            in m.maybeSelected
              |> M.unwrap
                TileCleared
                (\(s, ls) ->
                  if v == s
                  then TileSelected
                  else case L.find (\(vm, _) -> vm == v) ls of
                    Nothing     -> TileCleared
                    Just (_, a) -> TileChecked a
                )
              |> tileView (SelectTile >> act) m.board (v, mp)
          )
          xs
        , [ rankBorderCellView r ]
        ]
      )
    )
    |> List.reverse
    |> List.append
      ( if cp
        then
          [ div
            [ style "position" "absolute"
            , style "backgroundColor" "black"
            , style "height" "100%"
            , style "opacity" ".2"
            , style "width" "100%"
            , style "z-index" "2"
            ]
            []
          ]
        else []
      )
    |> div
      [ style "position" "relative"
      , style "border" "none"
      , if cp then style "pointer-events" "none" else emptyAttribute
      ]

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