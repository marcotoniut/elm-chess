module View.Game exposing (..)

import Chess.Base exposing (..)
import Direction exposing (V2)
import Result.Extra as R
import View.Base exposing (..)

type alias GamePlayer =
  { name : String
  , id : String
  }

type GameModel
  = LoadingGame
  | OnePlayer
    { white : GamePlayer
    }
  | GameStart
    { white : GamePlayer
    , black : GamePlayer
    , game : Game
    }
  | InvalidGame
    { error : PlayError
    }


resetInputs : GameInputs a -> GameInputs a
resetInputs a =
  { a
  | choosingPromotion = Nothing
  , maybeSelected = Nothing
  }

choosePromotion : PawnPromotion ->  WithGame (GameInputs a) ->  WithGame (GameInputs a)
choosePromotion pr m =
  let pl = gameTurn m.game
  in case m.choosingPromotion of
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
