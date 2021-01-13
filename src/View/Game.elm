module View.Game exposing (..)

import Chess.Base exposing (..)
import Direction exposing (V2)
import Result.Extra as R
import View.Base exposing (..)

resetInputs : GameInputs a -> GameInputs a
resetInputs a =
  { a
  | choosingPromotion = Nothing
  , maybeSelected = Nothing
  }

choosePromotion : PawnPromotion ->  HasGame (GameInputs a) ->  HasGame (GameInputs a)
choosePromotion pr m =
  case m.choosingPromotion of
    Nothing -> m
    Just cp ->
      case m.maybeSelected of
        Nothing -> m
        Just ((f, _), ms) ->
          m.game
          |> (case cp of
            ChoosingPromotionAdvance   ->
              tryMove (PawnPieceMove <| PawnPromotion pr <| PawnPromotionAdvance f) 
            ChoosingPromotionCapture d ->
              tryMove (PawnPieceMove <| PawnPromotion pr <| PawnPromotionCapture f d)
            )
          |> R.unwrap m (\g -> { m | game = g } |> resetInputs)
