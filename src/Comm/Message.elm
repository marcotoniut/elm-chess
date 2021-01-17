module Comm.Message exposing (..)

import Chess.Base exposing (Player(..))
import Json.Decode exposing (Decoder, field, string)
import Json.Decode as JD
import Json.Encode

type alias GamePlayer =
  { id : String
  , name : String
  -- , kind : Player
  }

playerDecoder : Decoder Player
playerDecoder = JD.andThen
  (\s -> case s of
    "White" -> JD.succeed White
    "Black" -> JD.succeed Black
    _ -> JD.fail "No valid Player Kind"
  ) JD.string

gamePlayerDecoder : Decoder GamePlayer
gamePlayerDecoder =
  JD.map2
    (\x y ->
      { id = x
      , name = y
      -- , kind = z
      }
    )
    (JD.field "id" JD.string)
    (JD.field "name" JD.string)
    -- (JD.field "kind" playerDecoder)

type IncomingMessage
  = IncomingString String
  | IncomingPlayerJoin
    { channelId : String
    , player : GamePlayer
    }
  | IncomingGameStart
    { channelId : String
    , white : GamePlayer
    , black : GamePlayer
    }
  | IncomingPlayerJoined GamePlayer
  | IncomingPlayerLeft GamePlayer
  | IncomingANPieceMove String

inWsStringDecoder : Decoder IncomingMessage
inWsStringDecoder =
  JD.map IncomingString JD.string

inGamePlayerJoinDecoder : Decoder IncomingMessage
inGamePlayerJoinDecoder =
  JD.map2
    (\x y -> IncomingPlayerJoin
      { channelId = x
      , player = y
      }
    )
    (JD.field "channel_id" JD.string)
    (JD.field "player" gamePlayerDecoder)

inGameStartDecoder : Decoder IncomingMessage
inGameStartDecoder =
  JD.map3
    (\x y z -> IncomingGameStart
      { channelId = x
      , white = y
      , black = z
      }
    )
    (JD.field "channel_id" JD.string)
    (JD.field "white" gamePlayerDecoder)
    (JD.field "black" gamePlayerDecoder)

inGamePlayerLeftDecoder : Decoder IncomingMessage
inGamePlayerLeftDecoder =
  JD.map IncomingPlayerLeft gamePlayerDecoder

inANPieceMoveDecoder : Decoder IncomingMessage
inANPieceMoveDecoder =
  JD.map IncomingANPieceMove JD.string

incomingDecoder : Decoder IncomingMessage
incomingDecoder
  = JD.field "stage" JD.string
    |> JD.andThen
      (\s ->  JD.field "payload" <| case s of
        "WsString"       -> inWsStringDecoder
        "GamePlayerJoin" -> inGamePlayerJoinDecoder
        "GameStart"      -> inGameStartDecoder
        "GamePlayerLeft" -> inGamePlayerLeftDecoder
        "ANPieceMove"    -> inANPieceMoveDecoder
        _ -> JD.fail ("Invalid WsMessage stage: " ++ s)
      )

-- TODO
type OutgoingMessage
  = DisconnectRequest
  | PieceMoveMessage String

outgoingEncode : OutgoingMessage -> String
outgoingEncode m = case m of
  DisconnectRequest  -> "_______"
  PieceMoveMessage s -> "w" ++ s

-- outgoingEncode : OutgoingMessage -> Json.Encode.Value
-- outgoingEncode m = case m of
--   DisconnectRequest  ->
--     Json.Encode.object [ ( "stage", Json.Encode.string "DisconnectRequest" ) ]
--   PieceMoveMessage s ->
--     Json.Encode.object
--       [ ( "stage", Json.Encode.string "PieceMoveMessage" )
--       , ( "payload"
--         , Json.Encode.object
--           [ ( "move", Json.Encode.string s ) ]
--         )
--       ]