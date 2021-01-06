module View.Game exposing (..)

import Chess.Base exposing (Game, PlayError)

type alias GamePlayer =
  { name : String
  , id : String
  }

type GameModel
  = OnePlayer
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
