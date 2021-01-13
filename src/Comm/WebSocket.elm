module Comm.WebSocket exposing (..)

import Basics.Extra as B
import Lib.PortFunnels exposing (FunnelDict, Handler(..), State)
import Lib.PortFunnels as PF
import PortFunnel.WebSocket as WS exposing (Response(..))

type alias WebSocket =
  { log : List String
  , useSimulator : Bool
  , wasLoaded : Bool
  , state : State
  , error : Maybe String
  }

type alias HasWebSocket a = { a | ws : WebSocket }

setWs : WebSocket -> HasWebSocket a -> HasWebSocket a
setWs ws a = { a | ws = ws }

asWsIn : HasWebSocket a -> WebSocket -> HasWebSocket a
asWsIn = B.flip setWs

appendLog : i -> { a | log : List i } -> { a | log : List i }
appendLog i a = { a | log = i :: a.log }

initWebSocket : WebSocket
initWebSocket =
  { log = []
  , useSimulator = False
  , wasLoaded = False
  , state = PF.initialState
  , error = Nothing
  }

doIsLoaded : WebSocket -> WebSocket
doIsLoaded ws =
  if not ws.wasLoaded && WS.isLoaded ws.state.websocket
  then
    { ws
    | useSimulator = False
    , wasLoaded = True
    }
  else ws

closedString : WS.ClosedCode -> Bool -> Bool -> String
closedString code wasClean expected
  = "code: "
  ++ WS.closedCodeToString code
  ++ ", "
  ++ (if wasClean
      then "clean"
      else "not clean"
      )
  ++ ", "
  ++ (if expected
      then "expected"
      else "NOT expected"
      )
