module Model exposing (..)

import Chess.Base exposing (Game, PlayError)

type alias GamePlayer =
  { id : String
  , name : String
  }

type alias GameOnePlayerState =
  { white : GamePlayer
  }

type alias GameInProgressState =
  { white : GamePlayer
  , black : GamePlayer
  , game : Game
  }

type alias GameErrorState =
  { error : PlayError
  }

type GameState
  = GameIdling
  | GameOnePlayer GameOnePlayerState
  | GameInProgress GameInProgressState
  | GameError GameErrorState
  | GameFinished
