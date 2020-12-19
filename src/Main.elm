module Main exposing (..)

import Array
import Browser
import Direction exposing (..)
import Chess exposing (..)
import Component exposing (blank)
import Composition exposing (standardComposition, castlingComposition)
import Debug
import Html.Attributes exposing (width, height, style, disabled, title)
import Html exposing (Html, button, br, node, div, ul, li, span, text, input)
import Html.Events exposing (onInput, onClick)
import Icon exposing (pieceToIcon)
import Matrix
import List.Extra as L
import Maybe.Extra as M
import Result.Extra as R
import Theme exposing (
    darkSpaceColor, darkSpaceColor
  , lightSpaceColor, borderColor, whitePlayerColor
  , blackPlayerColor, checkSize
  )
import Component exposing (emptyAttribute)
import PawnPromotion as PP
import View.Base exposing (..)
import View.Tile exposing (..)

-- MODEL
type alias Model =
  { input : String
  , moves : List PieceMove
  , gameState : Result PlayError Game
  , maybeSelected : Maybe (V2, List (V2, AvailableMove))
  , choosingPromotion : Maybe ChoosingPromotion
  }

init : Model
init =
  { gameState = Result.Ok
    { board = List.foldl
        (\(Tile (x, y) p) g -> Matrix.set (x, y) (Just p) g)
        initBoard
        -- standardComposition
        castlingComposition
    , moves = []
    , blackCastlingAvailable = castlingEnabled
    , whiteCastlingAvailable = castlingEnabled
    }
  , input = ""
  , moves = []
  , maybeSelected = Nothing
  , choosingPromotion = Nothing
  }

-- MAIN
main : Program () Model Msg
main = Browser.sandbox { init = init, update = update, view = view }

-- UPDATE
type Msg
  = ChangeInput String
  | MovePiece PieceMove
  | SelectTile V2
  | ChoosePromotion PawnPromotion
  | UndoPieceMove


-- TODO PawnPromotion
update : Msg -> Model -> Model
update msg model = case msg of
  ChangeInput i -> { model | input = i }
  MovePiece m   ->
    { model
    | gameState = Result.andThen (play [ m ]) model.gameState
    , moves = m :: model.moves
    }
  UndoPieceMove ->
    { model
    | gameState
      = Result.map
      (\g ->
        case g.moves of
          [] -> g
          (m, b) :: rs ->
            { g
            | board = b
            , moves = rs
            }
      ) model.gameState
    , moves
      = model.moves
      |> List.tail
      |> Maybe.withDefault []
    }
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
                  let ns = tryMove m g
                  in
                    { model
                      | gameState = ns
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


-- VIEW
view : Model -> Html Msg
view model =
  div
    [ style "display" "flex" ]
    [ let cp = M.isJust model.choosingPromotion
      in div
      [ style "display" "flex"
      , style "flex-direction" "column"
      ]
      ([ model.gameState
        |> Result.map
          (\g -> g.board
          |> Matrix.toList
          |> List.indexedMap
            (\r xs -> div
              [ style "display" "flex"
              ]
              (List.indexedMap
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
            )
          |> List.reverse
          |> List.append
            ( if cp
              then
                [ div
                  [ style "position" "absolute"
                  , style "height" "100%"
                  , style "width" "100%"
                  , style "backgroundColor" "black"
                  , style "opacity" ".2"
                  , style "z-index" "2"
                  ]
                  []
                ]
              else []
            )
          |> div
            [ style "position" "relative"
            , style "borderColor" borderColor
            , style "borderStyle" "solid"
            , style "borderWidth" "35px"
            , if cp then style "pointer-events" "none" else emptyAttribute
            ]
          )
        |> Result.mapError (\e -> div [] [ text (Debug.toString e) ])
        |> R.merge
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
      [ button
        [ onClick UndoPieceMove
        , disabled <| List.isEmpty model.moves
        ]
        [ text "UNDO" ]
      , R.unwrap
          blank
          (\g -> div []
            [ text <| "Turn " ++ Debug.toString (gameTurn g)
            , br [] []
            , text <| Debug.toString g
            , br [] []
            , text <| Debug.toString model.input
            , br [] []
            , text <| Debug.toString model.maybeSelected
            , br [] []
            , text <| Debug.toString model.choosingPromotion
            ]
          )
          model.gameState
      , div
        [ style "display" "grid"
        , style "grid-gap" "1em"
        , style "grid-template-columns" "repeat(3, 1fr)"
        , style "margin" "1em"
        ]
        (List.map
          ((|>) model.gameState)
          [ moveButton (KingPieceMove (KingCastling KingSide))
          , moveButton (KingPieceMove (KingCastling QueenSide))
          , moveButton (PawnPieceMove (PawnAdvance (3, 1)))
          , moveButton (PawnPieceMove (PawnDoubleAdvance 6))
          , moveButton (PawnPieceMove (PawnEnPassant Right))
          , moveButton (BishopPieceMove (BishopMove (0, 3) NE 1))
          , moveButton (PawnPieceMove (PawnPromotion QueenPromotion (PawnPromotionAdvance 2)))
          , moveButton (PawnPieceMove (PawnPromotion KnightPromotion (PawnPromotionCapture 2 Right)))
          ]
        )
      , div
        [ style "margin" "1em"
        , style "border" "1px solid black"
        ]
        [ input
          [ onInput ChangeInput
          , style "backgroundColor" "lightyellow"
          , style "border" "none"
          , style "border-radius" "0"
          , style "box-sizing" "border-box"
          , style "border-bottom" "1px solid black"
          , style "width" "100%"
          ] []
        , model.moves
          |> List.indexedMap
            -- (\i m -> toAN b pl m
            (\i ->
              moveText
              >> List.singleton
              >> li [ style "backgroundColor" <| if modBy 2 i == 0 then "white" else "lightgrey" ]
            )
          |> ul
            [ style "list-style" "none"
            , style "min-height" "100px"
            , style "padding" "0"
            , style "margin" "0"
            ]
        ]
      ]
    ]

moveButton : PieceMove -> Result PlayError Game -> Html Msg
moveButton m rg =
  button
  (
    [
      [ onClick (MovePiece m) ]
    , Result.andThen (play [ m ]) rg
      |> R.unpack
        (\e ->
          [ disabled True
          , title <| Debug.toString e
          ]
        )
        (always [])
    ] |> List.concat
  )
  [ moveText m ]

moveText : PieceMove -> Html a
moveText = Debug.toString >> text 
