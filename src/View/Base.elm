module View.Base exposing (..)

import Chess.Base exposing (..)
import Direction exposing (..)

type ChoosingPromotion
  = ChoosingPromotionAdvance
  | ChoosingPromotionCapture HorizontalDirection

type AvailableMove
  = AvailablePieceMove PieceMove
  | AvailablePawnPromotionMove PawnPromotionMove

type alias HasPlayer a =
  { a | player : Player }

type alias HasGame a =
  { a | game : Game }

type alias HasMaybeSelected a =
  { a | maybeSelected : Maybe (V2, List (V2, AvailableMove)) }

type alias GameInputs a = HasMaybeSelected
  ({ a | choosingPromotion : Maybe ChoosingPromotion })
