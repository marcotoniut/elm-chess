module Main exposing (..)

import Alphabet exposing (intToAlphabet)
import Array
import Browser
import Browser.Navigation as Navigation
import Basics.Extra as B
import Direction exposing (..)
import Chess.AlgebraicNotation exposing (..)
import Chess.Base exposing (..)
import Chess.Composition exposing (standardComposition, castlingComposition)
import Cmd.Extra exposing (addCmd, addCmds, withCmd, withCmds, withNoCmd)
import Component exposing (blank, emptyAttribute)
import Debug
import Html exposing (Html, a, button, br, node, div, ul, li, span, text, input)
import Html.Attributes exposing (width, height, style, disabled, title)
import Html.Events exposing (onInput, onClick)
import Json.Encode exposing (Value)
import List.Extra as L
import Matrix
import Maybe.Extra as M
import PortFunnel.WebSocket as WS exposing (Response(..))
import PortFunnels exposing (FunnelDict, Handler(..), State)
import Result.Extra as R
import Theme exposing (..)
import Url as Url
import Url.Parser exposing (Parser, (</>), int, map, oneOf, parse, s, string)
import View.Base exposing (..)
import View.Tile exposing (..)
import View.Debug.MoveCommands exposing (..)
import View.MoveHistory exposing (..)
import View.PawnPromotion as PP
import Tuple2 as T

handlers : List (Handler Model Msg)
handlers =
  [ WebSocketHandler (\r s m -> socketHandler r s m.ws |> Tuple.mapFirst (asWsIn m))
  ]

subscriptions : m -> Sub Msg
subscriptions = PortFunnels.subscriptions Process
-- subscriptions = 
--   Sub.batch
--     [ subPort Receive
--     , parseReturn Process
--     ]

funnelDict : FunnelDict Model Msg
funnelDict = PortFunnels.makeFunnelDict handlers getCmdPort


{-| Get a possibly simulated output port. -}
getCmdPort : String -> Model -> (Value -> Cmd Msg)
getCmdPort n m = PortFunnels.getCmdPort Process n m.ws.useSimulator

{-| The real output port. -}
cmdPort : Value -> Cmd Msg
cmdPort = PortFunnels.getCmdPort Process "" False

type alias WebSocket =
  { log : List String
  , useSimulator : Bool
  , wasLoaded : Bool
  , state : State
  , error : Maybe String
  }

type alias Model =
  { input : String
  , moves : List PieceMove
  , gameState : Result PlayError Game
  , maybeSelected : Maybe (V2, List (V2, AvailableMove))
  , choosingPromotion : Maybe ChoosingPromotion
  -- , channel : String
  -- , navKey : Nav.Key
  , route : Maybe Route
  , player : String
  , opponent : Maybe String
  , ws : WebSocket
  }

setWs : WebSocket -> { a | ws : WebSocket } -> { a | ws : WebSocket }
setWs ws a = { a | ws = ws }

appendLog : i -> { a | log : List i } -> { a | log : List i }
appendLog i a = { a | log = i :: a.log }

asWsIn : { a | ws : WebSocket } -> WebSocket -> { a | ws : WebSocket }
asWsIn = B.flip setWs

consWsUrl : String -> String -> String
consWsUrl channel name = "ws://localhost:8080/channel/" ++ channel ++ "?name=" ++ name
-- wsUrl _ _ = "wss://echo.websocket.org"


wsKey : String
wsKey = "socket"

type Route
  = ChessRoute String

routeParser : Parser (Route -> a) a
routeParser =
  oneOf
    [ map ChessRoute   (s "chess" </> string)
    ]

init : flags -> Url.Url -> Navigation.Key -> (Model, Cmd Msg)
init _ url _ =
  let ws =
        { log = []
        , useSimulator = False
        , wasLoaded = False
        , state = PortFunnels.initialState
        , error = Nothing
        }
      model =
        { gameState = Result.Ok
          { board = List.foldl
              (\(Tile (x, y) p) g -> Matrix.set (x, y) (Just p) g)
              initBoard
              standardComposition
          , moves = []
          , blackCastlingAvailable = castlingEnabled
          , whiteCastlingAvailable = castlingEnabled
          }
        , input = ""
        , moves = []
        , maybeSelected = Nothing
        , choosingPromotion = Nothing
        , route = parse routeParser url
        , player = "Marco"
        , opponent = Nothing
        , ws = ws
        }
  in model
  |> M.unwrap
    withNoCmd
    (\r -> case r of
      ChessRoute c ->
        let wsUrl = consWsUrl c model.player
        in withCmd (WS.makeOpenWithKey wsKey wsUrl |> send model)
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
  | SelectTile V2
  | ChoosePromotion PawnPromotion
  | Process Value
  | Close
  | Connect


update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
  Connect ->
    let ws  = model.ws
        url = consWsUrl "111" "Marco"
    in appendLog
        (if ws.useSimulator
        then "Connecting to simulator"
        else "Connecting to " ++ url
        ) model.ws
      |> asWsIn model
      |> withCmd (WS.makeOpenWithKey wsKey url |> send model)
      -- |> withCmd (WS.makeOpen url |> send model)
  Close ->
    appendLog "Closing" model.ws
    |> asWsIn model
    |> withCmd (WS.makeClose wsKey |> send model)
  Process value ->
    case PortFunnels.processValue funnelDict value model.ws.state model of
      Err error ->
        let ws = model.ws
        in { ws | error = Just error }
          |> asWsIn model
          |> withNoCmd
      Ok res -> res
  ChangeInput i -> { model | input = i } |> withNoCmd
  MovePiece m   ->
    model.gameState
    |> Result.andThen
      (\g ->
        let an = toAN m g |> Result.toMaybe
        in play [ m ] g
          |> Result.map
            (\ng ->
              model.ws
              |> appendLog ("Sending \"" ++ Debug.toString m ++ "\"")
              |> asWsIn { model | gameState = Result.Ok ng }
              |> M.unwrap
                withNoCmd
                (WS.makeSend wsKey >> send model >> withCmd)
                an
            )
      )
    |> Result.withDefault (model |> withNoCmd)
  SelectTile v -> R.unwrap model
    (\g ->
      let pl = gameTurn g
      in case model.maybeSelected of
        Nothing ->
          Matrix.get v g.board
          |> M.join
          |> M.unwrap model
            (\p  ->
              if piecePlayer p == pl
              then
                { model
                | maybeSelected
                  = Just
                    (v
                    , List.concat
                      [ pieceLegalMoves g v p
                        |> List.map (Tuple.mapSecond AvailablePieceMove)
                      , pawnLegalPromotionMoves v g
                        |> List.map (Tuple.mapSecond AvailablePawnPromotionMove)
                      ]
                    )
                }
              else model
            )
        Just (s, ms) ->
          if s == v
          then { model | maybeSelected = Nothing }
          else case L.find (\(x, _) -> x == v) ms of
            Nothing ->
              case Matrix.get v g.board |> M.join of
                Nothing -> { model | maybeSelected = Nothing }
                Just p  ->
                  if piecePlayer p == pl
                  then
                    { model
                    | maybeSelected
                      = Just
                        (v
                        , List.concat
                          [ pieceLegalMoves g v p
                            |> List.map (Tuple.mapSecond AvailablePieceMove)
                          , pawnLegalPromotionMoves v g
                            |> List.map (Tuple.mapSecond AvailablePawnPromotionMove)
                          ]
                        )
                    }
                  else { model | maybeSelected = Nothing }
            Just (_, a) ->
              case a of
                AvailablePieceMove m ->
                  { model
                  | gameState = tryMove m g
                  , moves     = m :: model.moves
                  , maybeSelected = Nothing
                  }
                AvailablePawnPromotionMove m ->
                  { model
                  | choosingPromotion = Just <| case m of
                    PawnPromotionAdvance _ -> ChoosingPromotionAdvance
                    PawnPromotionCapture _ d -> ChoosingPromotionCapture d
                  }
    ) model.gameState
    |> withNoCmd
  ChoosePromotion pr -> R.unwrap model
    (\g ->
      let pl = gameTurn g
      in case model.choosingPromotion of
        Nothing -> model
        Just cp ->
          case model.maybeSelected of
            Nothing -> model
            Just (v, ms) ->
              let f = Tuple.first v
              in case cp of
                ChoosingPromotionAdvance   ->
                  let m  = PawnPieceMove <| PawnPromotion pr <| PawnPromotionAdvance f
                      ns = tryMove m g
                  in
                    { model
                    | gameState = ns
                    , moves     = m :: model.moves
                    , maybeSelected = Nothing 
                    , choosingPromotion = Nothing
                    }
                ChoosingPromotionCapture d ->
                  let m  = PawnPieceMove <| PawnPromotion pr <| PawnPromotionCapture f d
                      ns = tryMove m g
                  in
                    { model
                    | gameState = ns
                    , moves     = m :: model.moves
                    , maybeSelected = Nothing 
                    , choosingPromotion = Nothing
                    }

    ) model.gameState
    |> withNoCmd

view : Model -> Browser.Document Msg
view model =
  { title = "Chess"
  , body =
    [ mainView model
    -- , h2 [] [ text title ]
    ]
  }

-- VIEW
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
        , model.gameState
        |> Result.map
          (\g -> g.board
          |> Matrix.toList
          |> List.indexedMap
            (\r xs -> div
              [ style "margin" "0 auto"
              , style "display" "flex"
              -- , style "width" "640px"
              -- , style "height" "640px"
              -- , style "display" "grid"
              -- , style "grid-template-columns" "repeat(8, 1fr)"
              ]
              (List.concat
                [ [ rankBorderCellView r ]
                , (List.indexedMap
                    (\f ->
                      let v = (f, r)
                      in model.maybeSelected
                        |> M.unwrap
                          TileCleared
                          (\(s, ls) ->
                            if v == s
                            then TileSelected
                            else case L.find (\(vm, _) -> vm == v) ls of
                              Nothing     -> TileCleared
                              Just (_, m) -> TileChecked m
                          )
                        |> tileView SelectTile g.board v
                    ) xs
                  )
                , [ rankBorderCellView r ]
                ]
              )
            )
          |> List.reverse
          |> List.append
            ( if cp
              then
                [ div
                  [ style "position" "absolute"
                  , style "backgroundColor" "black"
                  , style "height" "100%"
                  , style "opacity" ".2"
                  , style "width" "100%"
                  , style "z-index" "2"
                  ]
                  []
                ]
              else []
            )
          |> div
            [ style "position" "relative"
            , style "border" "none"
            , if cp then style "pointer-events" "none" else emptyAttribute
            ]
          )
        |> Result.mapError (\e -> div [] [ text (Debug.toString e) ])
        |> R.merge
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
      , R.unwrap
          blank
          (\g -> div []
            [ text <| "Turn " ++ Debug.toString (gameTurn g)
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
              , moveHistoryView g
              ]
            ]
          )
          model.gameState
      , moveCommandsView MovePiece model.gameState
      ]
    ]

rankBorderCellView : Int -> Html a
rankBorderCellView i =
  div
  [ style "width" (intToPx (tileSize // 2))
  , style "display" "flex"
  , style "justify-content" "center"
  , style "align-items" "center"
  , style "background-color" borderColor
  ]
  [ text <| String.fromInt <| i + 1 ]


fileBorderRowView : Int -> Html a
fileBorderRowView n =
  div
  [ style "background-color" borderColor
  , style "display" "flex"
  , style "height" (intToPx (tileSize // 2))
  , style "margin" "0 auto"
  , style "padding" ("0 " ++ intToPx (tileSize // 2))
  , style "text-align" "center"
  , style "v-align" "center"
  ]
  (List.range 0 (n - 1) |> List.map
    (intToAlphabet
    >> text
    >> List.singleton
    >> div
      [ style "width" (intToPx tileSize)
      , style "display" "flex"
      , style "justify-content" "center"
      , style "align-items" "center"
      ]
    )
  )


  -- WEBSOCKET
send : Model -> WS.Message -> Cmd Msg
send m = WS.send (getCmdPort WS.moduleName m)


doIsLoaded : WebSocket -> WebSocket
doIsLoaded ws =
  if not ws.wasLoaded && WS.isLoaded ws.state.websocket
  then
    { ws
    | useSimulator = False
    , wasLoaded = True
    }
  else ws


socketHandler : Response -> State -> WebSocket -> (WebSocket, Cmd Msg)
socketHandler response state m =
  let model = doIsLoaded
        { m
        | state = state
        , error = Nothing
        }
  in case response of
    WS.MessageReceivedResponse { message } ->
      model
      |> appendLog ("Received \"" ++ message ++ "\"")
      |> withNoCmd
    WS.ConnectedResponse r ->
      model
      |> appendLog ("Connected: " ++ r.description)
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
        |> appendLog ("Reconnected: " ++ r.description)
        |> withNoCmd
      xs ->
        model
        |> appendLog (Debug.toString xs)
        |> withNoCmd

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
