module Main exposing (..)

import Browser
import Browser.Navigation as Navigation
import Chess.Base exposing (..)
import Cmd.Extra exposing (addCmd, addCmds, withCmd, withCmds, withNoCmd)
import Comm.Message exposing (..)
import Comm.WebSocket exposing (..)
import Component exposing (blank, emptyAttribute)
import Debug
import Html exposing (Html, a, button, br, node, div, ul, li, span, text, input)
import Html.Attributes exposing (width, height, style, disabled, title)
import Html.Events exposing (onInput, onClick)
import Json.Decode as JD
import Json.Encode exposing (Value)
import Lib.PortFunnels exposing (FunnelDict, Handler(..), State)
import Lib.PortFunnels as PF
import Maybe.Extra as M
import Model exposing (..)
import PortFunnel.WebSocket as WS exposing (Response(..))
import Result.Extra as R
import Screen.Multiplayer exposing (..)
import Url as Url
import Url.Parser exposing (Parser, (</>), (<?>), int, map, oneOf, parse, s, string)
import Url.Parser.Query as Query
import View.Base exposing (..)
import View.Board exposing (..)
import View.Debug.MoveCommands exposing (..)
import View.MoveHistory exposing (..)
import View.PawnPromotion as PP
import View.Game exposing (..)

handlers : List (Handler Model Msg)
handlers =
  [ WebSocketHandler (\r s m -> socketHandler r s m.ws |> Tuple.mapFirst (asWsIn m))
  ]

subscriptions : m -> Sub Msg
subscriptions = PF.subscriptions Process

funnelDict : FunnelDict Model Msg
funnelDict = PF.makeFunnelDict handlers getCmdPort


{-| Get a possibly simulated output port. -}
getCmdPort : String -> Model -> (Value -> Cmd Msg)
getCmdPort n m = PF.getCmdPort Process n m.ws.useSimulator

{-| The real output port. -}
cmdPort : Value -> Cmd Msg
cmdPort = PF.getCmdPort Process "" False


type alias Model =
  GameInputs (
    { input : String
    , initialBoard : Board
    , gameState : GameState
    -- , channel : String
    -- , navKey : Nav.Key
    , route : Maybe Route
    -- , player : Maybe String
    , opponent : Maybe String
    , ws : WebSocket
    }
  )

consWsUrl : String -> String -> String
consWsUrl channel name = "ws://localhost:8080/channel/" ++ channel ++ "?name=" ++ name
-- wsUrl _ _ = "wss://echo.websocket.org"

wsKey : String
wsKey = "socket"

type Route
  = ChessRoute String (Maybe String)
  -- = LobbyRoute String

routeParser : Parser (Route -> a) a
routeParser =
  oneOf
    [ map ChessRoute (s "chess" </> string <?> Query.string "name")
    ]

init : flags -> Url.Url -> Navigation.Key -> (Model, Cmd Msg)
init _ url _ =
  let ws = initWebSocket
      model =
        { initialBoard = initBoard
        , gameState = GameIdling
        , input = ""
        , maybeSelected = Nothing
        , choosingPromotion = Nothing
        , route = parse routeParser url
        , opponent = Nothing
        , ws = ws
        }
  in M.unwrap
    (withNoCmd model)
    (\r -> case r of
      ChessRoute c pl ->
        let wsUrl = consWsUrl c (Maybe.withDefault "" pl)
        in appendLog ("Connecting to " ++ wsUrl) model.ws
        |> asWsIn model
        |> withCmd (WS.makeOpenWithKey wsKey wsUrl |> send model)
    )
    model.route

main : Program () Model Msg
main = Browser.application
  { init = init
  , update = update
  , view = view
  , subscriptions = subscriptions
  , onUrlRequest = (\_ -> Close)
  , onUrlChange = (\_ -> Close)
  }

type Msg
  = ChangeInput String
  | MovePiece PieceMove
  | BoardAction BoardAction
  | ChoosePromotion PawnPromotion
  | Process Value
  | Close
  | Connect

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
  Connect ->
    M.unwrap
      (withNoCmd model)
      (\r -> case r of
        ChessRoute c pl ->
          let wsUrl = consWsUrl c (Maybe.withDefault "" pl)
          in appendLog ("Connecting to " ++ wsUrl) model.ws
          |> asWsIn model
          |> withCmd (WS.makeOpenWithKey wsKey wsUrl |> send model)
      )
      model.route
  Close ->
    appendLog "Closing" model.ws
    |> asWsIn model
    |> withCmd (WS.makeClose wsKey |> send model)
  Process value ->
    case PF.processValue funnelDict value model.ws.state model of
      Err error ->
        let ws = model.ws
        in { ws | error = Just error }
          |> asWsIn model
          |> withNoCmd
      Ok res -> res
  ChangeInput i -> { model | input = i } |> withNoCmd
  MovePiece m   ->
    (case model.gameState of
      GameInProgress s -> 
        play [ m ] s.game
        |> R.unwrap
          model
          (\g ->
            { model
            | gameState = GameInProgress
              { s | game = g }
            }
          )
      _ -> model
    )
    |> withNoCmd
  BoardAction (SelectTile v) ->
    (case model.gameState of
      GameInProgress state ->
        selectTile
          v
          { game = state.game
          , player = White
          , choosingPromotion = model.choosingPromotion
          , maybeSelected = model.maybeSelected
          }
        |> (\nm ->
          { model
          | gameState = GameInProgress
            { state
            | game = nm.game
            }
          , choosingPromotion = nm.choosingPromotion
          , maybeSelected = nm.maybeSelected
          })
      _ -> model
    ) |> withNoCmd
  ChoosePromotion pr -> 
    (case model.gameState of
      GameInProgress state ->
        choosePromotion
          pr
          { game = state.game
          , choosingPromotion = model.choosingPromotion
          , maybeSelected = model.maybeSelected
          }
        |> (\nm ->
            { model
            | gameState = GameInProgress
              { state
              | game = nm.game
              }
            , choosingPromotion = nm.choosingPromotion
            , maybeSelected = nm.maybeSelected
            })
      _ -> model
    )
    |> withNoCmd

view : Model -> Browser.Document Msg
view model =
  { title = "Chess"
  , body =
    [ mainView model
    -- , h2 [] [ text title ]
    ]
  }

mainView : Model -> Html Msg
mainView model =
  div
    [ style "display" "flex" ]
    [ let cp = M.isJust model.choosingPromotion
      in div
      [ style "display" "flex"
      , style "flex-direction" "column"
      ]
      ([ fileBorderRowView 8
      , case model.gameState of
        GameInProgress { game } ->
          boardView BoardAction
            { board = game.board
            , choosingPromotion = model.choosingPromotion
            , maybeSelected = model.maybeSelected
            }
        x -> div [] [ text (Debug.toString x) ]
      , fileBorderRowView 8
      ]
      |> List.append
        ( if cp
          then
            [ div
              []
              [ PP.view ChoosePromotion ]
            ]
          else
            [ div
              [ style "height" "83px" ]
              []
            ]
        )
      )
    , div
      []
      [ div
        []
        [ button
          [ onClick Connect ]
          [ text "CONNECT" ]
        , button
          [ onClick Close ]
          [ text "CLOSE" ]
        , div [] [ text <| "WS Log " ++ Debug.toString model.ws.log ]
        , div [] [ text <| "WS Error " ++ Debug.toString model.ws.error ]
        ]
      , case model.gameState of
        GameInProgress { game } ->
          div
          []
          [ text <| "Model " ++ Debug.toString model
          , div
            [ style "margin" "1em"
            , style "border" "1px solid black"
            ]
            [ input
              [ onInput ChangeInput
              , style "backgroundColor" "lightyellow"
              , style "border" "none"
              , style "border-bottom" "1px solid black"
              , style "border-radius" "0"
              , style "box-sizing" "border-box"
              , style "width" "100%"
              ] []
            , moveHistoryView game
            ]
          ]
        _ -> blank
      , moveCommandsView
        MovePiece
        (case model.gameState of
          GameInProgress { game } -> Result.Ok game
          _ -> Result.Err ()
        )
      ]
    ]

send : Model -> WS.Message -> Cmd Msg
send m = WS.send (getCmdPort WS.moduleName m)

socketHandler : Response -> State -> WebSocket -> (WebSocket, Cmd Msg)
socketHandler response state m =
  let model = doIsLoaded
        { m
        | state = state
        , error = Nothing
        }
  in case response of
    -- WS.MessageReceivedResponse { message } ->
    WS.MessageReceivedResponse { key, message } ->
      JD.decodeString wsMessageDecoder message
      |> R.unwrap 
        (model |> withNoCmd)
        (\r ->
          (case r of
            WsString s -> model |> appendLog ("Received " ++ Debug.toString r)
            WsJoinedAsWhite s -> model
              -- { channelId : String
              -- , player : Player
              -- }
            WsGameStart s -> model
              -- { channelId : String
              -- , white : Player
              -- , black : Player
              -- }
            WsPlayerJoined pl -> model
            WsPlayerLeft pl -> model
            WsANPieceMove s -> model
          )
          -- |> appendLog ("Received \"" ++ message ++ "\"")
          |> appendLog ("Received " ++ Debug.toString r)
          |> withNoCmd
        )
    WS.ConnectedResponse r ->
      model
      |> appendLog ("Connected: " ++ Debug.toString r)
      -- |> appendLog ("Connected: " ++ r.description)
      |> withNoCmd
    WS.ClosedResponse { code, wasClean, expected } ->
      model
      |> appendLog ("Closed, " ++ closedString code wasClean expected)
      |> withNoCmd
    WS.ErrorResponse error ->
      model
      |> appendLog (WS.errorToString error)
      |> withNoCmd
    _ -> case WS.reconnectedResponses response of
      [] -> model |> withNoCmd
      [ ReconnectedResponse r ] ->
        model
        |> appendLog ("Connected: " ++ Debug.toString r)
        -- |> appendLog ("Reconnected: " ++ r.description)
        |> withNoCmd
      xs ->
        model
        |> appendLog (Debug.toString xs)
        |> withNoCmd
