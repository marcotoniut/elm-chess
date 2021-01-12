module Comm.Message exposing (..)

import Json.Decode exposing (Decoder, field, string)
import Json.Decode as JD

type alias Player =
  { id : String
  , name : String
  }

playerDecoder : Decoder Player
playerDecoder =
  JD.map2
    (\x y -> { id = x, name = y })
    (JD.field "id" JD.string)
    (JD.field "name" JD.string)

type WsMessage
  = WsString String
  | JoinedAsWhite
    { channelId : String
    , player : Player
    }
  | GameStart
    { channelId : String
    , white : Player
    , black : Player
    }
  | PlayerJoined Player
  | PlayerLeft Player
  | ANPieceMove String


wsStringDecoder : Decoder WsMessage
wsStringDecoder =
  JD.map WsString JD.string

joinedAsWhiteDecoder : Decoder WsMessage
joinedAsWhiteDecoder =
  JD.map2
    (\x y -> JoinedAsWhite
      { channelId = x
      , player = y
      }
    )
    (JD.field "channel_id" JD.string)
    (JD.field "player" playerDecoder)

gameStartDecoder : Decoder WsMessage
gameStartDecoder =
  JD.map3
    (\x y z -> GameStart
      { channelId = x
      , white = y
      , black = z
      }
    )
    (JD.field "channel_id" JD.string)
    (JD.field "white" playerDecoder)
    (JD.field "black" playerDecoder)

playerJoinedDecoder : Decoder WsMessage
playerJoinedDecoder =
  JD.map PlayerJoined playerDecoder

playerLeftDecoder : Decoder WsMessage
playerLeftDecoder =
  JD.map PlayerLeft playerDecoder

anPieceMoveDecoder : Decoder WsMessage
anPieceMoveDecoder =
  JD.map ANPieceMove JD.string

wsMessageDecoder : Decoder WsMessage
wsMessageDecoder
  = JD.field "stage" JD.string
    |> JD.andThen
      (\s ->  JD.field "payload" <| case s of
        "WsString" -> wsStringDecoder
        "JoinedAsWhite" -> joinedAsWhiteDecoder
        "GameStart" -> gameStartDecoder
        "PlayerJoined" -> playerJoinedDecoder
        "PlayerLeft" -> playerLeftDecoder
        "ANPieceMove" -> anPieceMoveDecoder
        _ -> JD.fail ("Invalid WsMessage stage: " ++ s)
      )

type alias ConnectRequest =
  { channel_id : String
  , self_id : String
  }

type alias DisconnectRequest =
  { id : String
  , channel_id : String
  }

type alias ClientActorMessage =
  { id : String
  , msg : String
  , channel_id : String
  }
