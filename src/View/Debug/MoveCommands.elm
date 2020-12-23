module View.Debug.MoveCommands exposing (..)

import Chess exposing (..)
import Direction exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Result.Extra as R

moveText : PieceMove -> Html a
moveText = Debug.toString >> text 

moveButton : (PieceMove -> m) -> PieceMove -> Result PlayError Game -> Html m
moveButton onClickHandle m rg =
  button
  ( [ [ onClick (onClickHandle m) ]
    , Result.andThen (play [ m ]) rg
      |> R.unpack
        (\e ->
          [ disabled True
          , title <| Debug.toString e
          ]
        )
        (always [])
    ] |> List.concat
  )
  [ moveText m ]

moveCommandsView : (PieceMove -> m) -> Result PlayError Game -> Html m
moveCommandsView onClickHandle s =
  let commandView = moveButton onClickHandle
  in div
  [ style "display" "grid"
  , style "grid-gap" "1em"
  , style "grid-template-columns" "repeat(3, 1fr)"
  , style "margin" "1em"
  ]
  (List.map
    ((|>) s)
    [ commandView (KingPieceMove (KingCastling KingSide))
    , commandView (KingPieceMove (KingCastling QueenSide))
    , commandView (PawnPieceMove (PawnAdvance (3, 1)))
    , commandView (PawnPieceMove (PawnDoubleAdvance 6))
    , commandView (PawnPieceMove (PawnEnPassant Right))
    , commandView (BishopPieceMove (BishopMove (0, 3) NE 1))
    , commandView (PawnPieceMove (PawnPromotion QueenPromotion (PawnPromotionAdvance 2)))
    , commandView (PawnPieceMove (PawnPromotion KnightPromotion (PawnPromotionCapture 2 Right)))
    ]
  )
