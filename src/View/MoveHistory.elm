module View.MoveHistory exposing (..)

import Chess.AlgebraicNotation exposing (..)
import Chess.Base exposing (..)
import Component exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import List.Extra as L
import Result.Extra as R

moveHistoryView : Game -> Html a
moveHistoryView =
  gameAN
  >> R.unwrap
    blank
    (  List.reverse
    >> L.greedyGroupsOf 2
    -- >> List.reverse
    >> List.indexedMap
      (\i xs ->
        li
        [ style "backgroundColor" "transparent" ]
        (xs |> List.map (text >> List.singleton >> span [ style "margin-right" "1rem" ]))
      )
    >> ol
      [ style "list-style-type" "decimal"
      , style "font-weight" "bold"
      -- , style "margin" "0"
      , style "min-height" "100px"
      -- , style "padding" "0"
      ]
    )

-- moveHistoryView : Game -> Html a
-- moveHistoryView =
--   gameAN
--   >> R.unwrap
--     blank
--     (List.indexedMap
--       (\i x ->
--         li
--         [ style "backgroundColor" <| if modBy 2 i == 0 then "white" else "lightgrey" ]
--         [ text x ]
--       )
--     >> ul
--       [ style "list-style" "none"
--       , style "margin" "0"
--       , style "min-height" "100px"
--       , style "padding" "0"
--       ]
--     )
