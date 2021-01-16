module Comm.Message exposing (..)

import Json.Decode exposing (Decoder, field, string)
import Json.Decode as JD
import Json.Encode

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

type IncomingMessage
  = IncomingString String
  | IncomingJoinedAsWhite
    { channelId : String
    , player : Player
    }
  | IncomingGameStart
    { channelId : String
    , white : Player
    , black : Player
    }
  | IncomingPlayerJoined Player
  | IncomingPlayerLeft Player
  | IncomingANPieceMove String


wsStringDecoder : Decoder IncomingMessage
wsStringDecoder =
  JD.map IncomingString JD.string

joinedAsWhiteDecoder : Decoder IncomingMessage
joinedAsWhiteDecoder =
  JD.map2
    (\x y -> IncomingJoinedAsWhite
      { channelId = x
      , player = y
      }
    )
    (JD.field "channel_id" JD.string)
    (JD.field "player" playerDecoder)

gameStartDecoder : Decoder IncomingMessage
gameStartDecoder =
  JD.map3
    (\x y z -> IncomingGameStart
      { channelId = x
      , white = y
      , black = z
      }
    )
    (JD.field "channel_id" JD.string)
    (JD.field "white" playerDecoder)
    (JD.field "black" playerDecoder)

playerJoinedDecoder : Decoder IncomingMessage
playerJoinedDecoder =
  JD.map IncomingPlayerJoined playerDecoder

playerLeftDecoder : Decoder IncomingMessage
playerLeftDecoder =
  JD.map IncomingPlayerLeft playerDecoder

anPieceMoveDecoder : Decoder IncomingMessage
anPieceMoveDecoder =
  JD.map IncomingANPieceMove JD.string

incomingDecoder : Decoder IncomingMessage
incomingDecoder
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

-- TODO
type OutgoingMessage
  = DisconnectRequest
  | PieceMoveMessage String

outgoingEncode : OutgoingMessage ->  Json.Encode.Value
outgoingEncode m =
  case m of
    DisconnectRequest  ->
      Json.Encode.object [ ( "stage", Json.Encode.string "DisconnectRequest" ) ]
    PieceMoveMessage s ->
      Json.Encode.object
        [ ( "stage", Json.Encode.string "PieceMoveMessage" )
        , ( "payload"
          , Json.Encode.object
            [ ( "move", Json.Encode.string  s ) ]
          )
        ]