module Main exposing (..)

import Alphabet exposing (intToAlphabet)
import Array
import Browser
import Direction exposing (..)
import Chess exposing (..)
import Component exposing (blank)
import Composition exposing (standardComposition, castlingComposition)
import Debug
import Html.Attributes exposing (width, height, style, disabled)
import Html exposing (Html, button, node, div, ul, li, span, text, input)
import Html.Events exposing (onInput, onClick)
import Icon exposing (pieceToIcon)
import Matrix
import Maybe.Extra as M
import Result.Extra as R
import Theme exposing (
    darkSpaceColor, darkSpaceColor
  , lightSpaceColor, borderColor, whitePlayerColor
  , blackPlayerColor, checkSize
  )
import Component exposing (emptyAttribute)

-- MODEL
type alias Model =
  { input : String
  , moves : List Move
  , gameR : Result MoveError Game
  , maybeSelected : Maybe V2
  }

init : Model
init =
  { gameR = Result.Ok
      { board = List.foldl
          (\(Tile (x, y) p) g -> Matrix.set (x, y) (Just p) g)
          initBoard
          -- standardComposition
          castlingComposition
      , blackCastlingAvailable = castlingEnabled
      , whiteCastlingAvailable = castlingEnabled
      , turn = White
      }
  , input = ""
  , moves = []
  , maybeSelected = Nothing
  }

-- MAIN
main : Program () Model Msg
main = Browser.sandbox { init = init, update = update, view = view }

-- UPDATE
type Msg
  = ChangeInput String
  | MovePiece Move
  | SelectTile V2

update : Msg -> Model -> Model
update msg model = case msg of
  SelectTile  v -> R.unwrap model
    (\g ->
      let mp = Matrix.get v g.board |> M.join
      in case model.maybeSelected of
        Nothing -> case mp of
          Nothing -> model
          Just p  ->
            if piecePlayer p == g.turn
            then { model | maybeSelected = Just v }
            else model
        Just s  ->
          if s == v
          then { model | maybeSelected = Nothing }
          else case mp of
            Nothing -> model
            Just p  ->
              if piecePlayer p == g.turn
              then { model | maybeSelected = Just v }
              else
                let m  = (PieceMove (Temp_TeleportMove s v))
                    ng = tryMove m g
                in if R.isOk ng
                then
                  { model | gameR = ng
                          , maybeSelected = Nothing 
                          , moves = m :: model.moves
                  }
                else model
    ) model.gameR
  ChangeInput i -> { model | input = i }
  MovePiece m   ->
    { model | gameR = Result.andThen (play [ m ]) model.gameR
            , moves = m :: model.moves
    }


-- VIEW
view : Model -> Html Msg
view model =
  div [ style "display" "flex" ]
    [ div []
      -- [ Result.andThen (\g -> play g model.moves) model.gameR
      [ model.gameR
        |> Result.map
          (\g ->
          let checkedMatrix = checkMatrix (Maybe.withDefault (-1, -1) model.maybeSelected) g.board
          in g.board
          |> Matrix.toList
          |> List.indexedMap
              (\i xs -> div
                [ style "display" "flex" ]
                (List.indexedMap
                  (\j ->
                    let ms = model.maybeSelected
                        v = (i, j)
                        ti = case ms of
                          Nothing -> TileCleared
                          Just s  ->
                            if (j, i) == s
                            then TileSelected
                            else TileCleared
                            -- else Matrix.get v checkedMatrix
                            --   |> M.unwrap TileCleared (always TileChecked) 
                    in tileView g.board v ti
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
    , div []
      [ R.unwrap
          blank
          (\g -> div [] [ text "Turn", text <| Debug.toString g.turn ])
          model.gameR
      , div
        [ style "display" "grid"
        , style "grid-gap" "1em"
        , style "grid-template-columns" "repeat(3, 1fr)"
        , style "margin" "1em"
        ]
        [ moveButton (Castling KingSide) model.gameR
        , moveButton (Castling QueenSide) model.gameR
        , moveButton (PieceMove (PawnPieceMove (PawnAdvance (4, 1)))) model.gameR
        , moveButton (PieceMove (Temp_TeleportMove (3, 1) (3, 3))) model.gameR
        , moveButton (PieceMove (Temp_TeleportMove (6, 1) (6, 3))) model.gameR
        , moveButton (PieceMove (Temp_TeleportMove (6, 7) (7, 5))) model.gameR
        , moveButton (PawnPromotion 2 2 QueenPromotion) model.gameR
        , moveButton (PawnPromotion 2 3 KnightPromotion) model.gameR
        ]
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

moveButton : Move -> Result MoveError Game -> Html Msg
moveButton m rg = button
  [ onClick (MovePiece m)
  , disabled <| R.isErr <| Result.andThen (play [ m ]) rg
  ]
  [ moveText m ]

moveText : Move -> Html a
moveText = Debug.toString >> text 

type TileInteraction
  = TileSelected
  | TileChecked
  | TileCleared

tileView : Board -> V2 -> TileInteraction -> Maybe Piece -> Html Msg
tileView b (i, j) t mp =
  let v = (j, i)
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
      [ style "backgroundColor"
        <| case t of
          TileSelected -> "mediumvioletred"
          TileChecked  -> "green"
          TileCleared  ->
            if List.isEmpty wcs
            then if List.isEmpty bcs then "transparent" else "blue"
            else if List.isEmpty bcs then "red"         else "magenta"
      , style "opacity" (if t == TileSelected then "1" else ".2")
      , style "position" "absolute"
      , style "bottom" "0"
      , style "left" "0"
      , style "right" "0"
      , style "top" "0"
      ]
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
