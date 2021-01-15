module Model exposing (..)

import Chess.Base exposing (Game, Player, PlayError)

type alias GamePlayer =
  { id : String
  , name : String
  }

type alias GameOnePlayerState =
  { player : Player
  , white : GamePlayer
  }

type alias GameInProgressState =
  { player : Player
  , white : GamePlayer
  , black : GamePlayer
  , game : Game
  }

type alias GameErrorState =
  { player : Player
  , error : PlayError
  }

type GameState
  = GameIdling
  | GameOnePlayer GameOnePlayerState
  | GameInProgress GameInProgressState
  | GameError GameErrorState
  | GameFinished
