module View.GameModel exposing (..)

import Chess.Base exposing (Game, PlayError)

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
