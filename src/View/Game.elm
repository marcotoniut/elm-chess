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

choosePromotion : PawnPromotion ->  HasGame (GameInputs a) -> (Maybe PieceMove, HasGame (GameInputs a))
choosePromotion pr m =
  case m.choosingPromotion of
    Nothing -> (Nothing, m)
    Just cp ->
      case m.maybeSelected of
        Nothing -> (Nothing, m)
        Just ((f, _), ms) ->
          let pm = case cp of
                ChoosingPromotionAdvance   ->
                  (PawnPieceMove <| PawnPromotion pr <| PawnPromotionAdvance f)
                ChoosingPromotionCapture d ->
                  (PawnPieceMove <| PawnPromotion pr <| PawnPromotionCapture f d)
          in m.game
          |> tryMove pm
          |> R.unwrap m (\g -> { m | game = g }) |> resetInputs |> Tuple.pair (Just pm)
