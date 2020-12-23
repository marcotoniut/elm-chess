module Main exposing (..)

import Array
import Browser
import Direction exposing (..)
import Chess exposing (..)
import Chess.AlgebraicNotation exposing (..)
import Chess.Composition exposing (standardComposition, castlingComposition)
import Component exposing (blank, emptyAttribute)
import Debug
import Html.Attributes exposing (width, height, style, disabled, title)
import Html exposing (Html, button, br, node, div, ul, li, span, text, input)
import Html.Events exposing (onInput, onClick)
import Icon exposing (pieceToIcon)
import List.Extra as L
import Matrix
import Maybe.Extra as M
import Result.Extra as R
import Theme exposing (
    darkSpaceColor, darkSpaceColor
  , lightSpaceColor, borderColor, whitePlayerColor
  , blackPlayerColor, checkSize
  )
import View.Base exposing (..)
import View.Tile exposing (..)
import View.Debug.MoveCommands exposing (..)
import View.PawnPromotion as PP

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
        standardComposition
        -- castlingComposition
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
    | gameState = Result.map (\g -> M.unwrap g Tuple.second (undoMove g)) model.gameState
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
            , style "border-color" borderColor
            , style "border-style" "solid"
            , style "border-width" "35px"
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
            -- , br [] []
            -- , text <| Debug.toString g
            -- , br [] []
            -- , text <| Debug.toString model.input
            , div
              [ style "margin" "1em"
              , style "border" "1px solid black"
              ]
              [ gameAN g
              |> R.unwrap
                blank
                (List.indexedMap
                  (\i x ->
                    li
                    [ style "backgroundColor" <| if modBy 2 i == 0 then "white" else "lightgrey" ]
                    [ text x ]
                  )
                >> ul
                  [ style "list-style" "none"
                  , style "margin" "0"
                  , style "min-height" "100px"
                  , style "padding" "0"
                  ]
                )]
            ]
          )
          model.gameState
      , moveCommands MovePiece model.gameState
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
        , model.moves
          |> List.indexedMap
            (\i ->
              moveText
              >> List.singleton
              >> li [ style "backgroundColor" <| if modBy 2 i == 0 then "white" else "lightgrey" ]
            )
          |> ul
            [ style "list-style" "none"
            , style "margin" "0"
            , style "min-height" "100px"
            , style "padding" "0"
            ]
        ]
      ]
    ]
