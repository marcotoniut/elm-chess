module View.Base exposing (..)

import Chess.Base exposing (..)
import Direction exposing (..)

type ChoosingPromotion
  = ChoosingPromotionAdvance
  | ChoosingPromotionCapture HorizontalDirection

type AvailableMove
  = AvailablePieceMove PieceMove
  | AvailablePawnPromotionMove PawnPromotionMove

type alias WithPlayer a =
  { a | player : Player }

type alias WithGame a =
  { a | game : Game }

type alias WithMaybeSelected a =
  { a | maybeSelected : Maybe (V2, List (V2, AvailableMove)) }

type alias GameInputs a = WithMaybeSelected
  ({ a | choosingPromotion : Maybe ChoosingPromotion })
