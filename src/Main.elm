module Main exposing (..)

import Alphabet exposing (intToAlphabet)
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

type ChoosingPromotion
  = ChoosingPromotionAdvance
  | ChoosingPromotionCapture HorizontalDirection

type AvailableMove
  = AvailablePieceMove PieceMove
  | AvailablePawnPromotionMove PawnPromotionMove

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


-- TODO PawnPromotion
update : Msg -> Model -> Model
update msg model = case msg of
  ChangeInput i -> { model | input = i }
  MovePiece m   ->
    { model | gameState = Result.andThen (play [ m ]) model.gameState
            , moves     = m :: model.moves
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
    -- { model | gameR = Result.andThen (play [ m ]) model.gameR
    --         , moves = m :: model.moves
    -- }


-- VIEW
view : Model -> Html Msg
view model =
  div
    [ style "display" "flex" ]
    [ div
      []
      [ model.gameState
        |> Result.map
          (\g -> g.board
          |> Matrix.toList
          |> List.indexedMap
            (\r xs -> div
              [ style "display" "flex" ]
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
                    |> tileView g.board v
                ) xs
              )
            )
          |> List.reverse
          |> div
            [ style "borderColor" borderColor
            , style "borderStyle" "solid"
            , style "borderWidth" "35px"
            , style "margin" "auto"
            ]
          )
        |> Result.mapError (\e -> div [] [ text (Debug.toString e) ])
        |> R.merge
       ]
    , div
      []
      [ R.unwrap
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
moveButton m rg = button
  ([
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

type TileInteraction
  = TileSelected
  | TileChecked AvailableMove
  | TileCleared

tileView : Board -> V2 -> TileInteraction -> Maybe Piece -> Html Msg
tileView b v t mp =
  let (i, j) = v
      wcs = inCheck White b v
      bcs = inCheck Black b v
  in div
    [ style "position" "relative"
    , style "backgroundColor"
      <| if (modBy 2 (i + j) == 0) then darkSpaceColor else lightSpaceColor
    , style "width" checkSize
    , style "height" checkSize
    , onClick <| SelectTile v
    ]
    [ div
      (List.concat
        [
          [ style "position" "absolute"
          , style "bottom" "0"
          , style "left" "0"
          , style "right" "0"
          , style "top" "0"
          ]
          , case t of
            TileSelected  ->
              [ style "backgroundColor" "mediumvioletred"
              , style "opacity" ".7"
              ]
            TileChecked m ->
              [ style "backgroundColor" "brown"
              , style "opacity" ".4"
              ]
            TileCleared   ->
              [ style "backgroundColor" "transparent"
              ]
              -- if List.isEmpty wcs
              -- then if List.isEmpty bcs then "transparent" else "blue"
              -- else if List.isEmpty bcs then "red"         else "magenta"
        ]
      )
      []
    , span
      [ style "position" "absolute"
      , style "top" "3px"
      , style "left" "3px"
      , style "user-select" "none"
      ]
      [ text <| intToAlphabet j ++ String.fromInt (i + 1) ]
    , mp
      |> Maybe.map
        (\p ->
          span
          [ style "position" "absolute"
          , style "left" "50%"
          , style "top" "50%"
          , style "transform" "translate(-50%, -50%)"
          , style "font-size" checkSize
          , style "user-select" "none"
          ]
          [ text <| pieceToIcon p ]
        )
      |> Maybe.withDefault blank
    ]
