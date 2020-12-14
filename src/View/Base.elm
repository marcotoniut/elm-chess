module View.Base exposing (..)

import Chess exposing (..)
import Direction exposing (..)

type ChoosingPromotion
  = ChoosingPromotionAdvance
  | ChoosingPromotionCapture HorizontalDirection

type AvailableMove
  = AvailablePieceMove PieceMove
  | AvailablePawnPromotionMove PawnPromotionMove
