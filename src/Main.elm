module Main exposing (..)

import Alphabet exposing (intToAlphabet)
import Array
import Browser
import Chess exposing (
    Board, Castling(..), Game, Move(..), Piece(..), PieceType(..), Player(..), Tile(..)
  , castlingEnabled, initBoard, other, play, tileInCheck
  )
import Component exposing (blank)
import Composition exposing (standardComposition, castlingComposition)
import Debug
import Html.Attributes exposing (width, height, style)
import Html exposing (Html, button, node, div, ul, li, span, text, input)
import Html.Events exposing (onInput, onClick)
import Matrix
import Result.Extra as R
import Chess exposing (MoveError)
import Chess exposing (PawnPromotion(..))

-- MODEL
type alias Model =
  { input : String
  , moves : List Move
  , gameR : Result MoveError Game
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
  }

-- MAIN
main : Program () Model Msg
main = Browser.sandbox { init = init, update = update, view = view }

-- UPDATE
type Msg
  = ChangeInput String
  | MovePiece Move

update : Msg -> Model -> Model
update msg model = case msg of
    ChangeInput i -> { model | input = i }
    MovePiece m ->
      { model | gameR = Result.andThen (\g -> play g [ m ]) model.gameR
              , moves = m :: model.moves
      }

darkSpaceColor   : String
darkSpaceColor   = "#769656" -- (118,150,86)
lightSpaceColor  : String
lightSpaceColor  = "#eeeed2" -- (238,238,210)
borderColor      : String
borderColor      = "#baca44" -- (186,202,68)
whitePlayerColor : String
whitePlayerColor = "#ffffff" -- (255,255,255)
blackPlayerColor : String
blackPlayerColor = "#000000" -- (0,0,0)
checkSize        : String
checkSize        = "70px"

-- VIEW
view : Model -> Html Msg
view model =
  div [ style "display" "flex" ]
    [ div []
      -- [ Result.andThen (\g -> play g model.moves) model.gameR
      [ model.gameR
        |> Result.map
          (\g ->
          g.board
          |> Matrix.toList
          >> List.indexedMap
              (\i xs -> div
                [ style "display" "flex" ]
                (List.indexedMap (tileView g.board i) xs)
              )
          >> List.reverse
          >> div
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
      [
        div
        [ style "display" "flex" ]
        [ button [ onClick (MovePiece (Castling KingSide)) ] [ text "Castle KingSide" ]
        , button [ onClick (MovePiece (Castling QueenSide)) ] [ text "Castle QueenSide" ]
        , button [ onClick (MovePiece (PieceMove (3, 1) (3, 3))) ] [ text "Move Pawn (3, 1) (3, 3)" ]
        , button [ onClick (MovePiece (PieceMove (6, 1) (6, 3))) ] [ text "Move Pawn (6, 1) (6, 3)" ]
        , button [ onClick (MovePiece (PieceMove (6, 7) (7, 5))) ] [ text "Move Knight (6, 7) (7, 5)" ]
        , button [ onClick (MovePiece (PawnPromotion White 2 2 QueenPromotion)) ] [ text "Promote White Pawn (Queen) 2 2" ]
        , button [ onClick (MovePiece (PawnPromotion White 2 3 KnightPromotion)) ] [ text "Promote White Pawn (Knight) 2 3" ]
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
            (\i m ->
              case m of
                PieceMove (f0, r0) (ff, rf) ->
                  li
                  [ style "backgroundColor" <| if modBy 2 i == 0 then "white" else "lightgrey" ]
                  [ text
                    <| "["   ++ Debug.toString f0 ++ " " ++ Debug.toString r0
                    ++ " - " ++ Debug.toString ff ++ " " ++ Debug.toString rf
                    ++ "]"
                  ]
                Castling c ->
                  li
                  [ style "backgroundColor" <| if modBy 2 i == 0 then "white" else "lightgrey" ]
                  [ text <| Debug.toString c ]
                PawnPromotion _ f0 ff p ->
                  li
                  [ style "backgroundColor" <| if modBy 2 i == 0 then "white" else "lightgrey" ]
                  [ text
                    <| "["    ++ Debug.toString f0 ++ " " ++ Debug.toString ff
                    ++ " -> " ++ Debug.toString p
                    ++ "]"
                  ]
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

pieceToIcon : Piece -> String
pieceToIcon p =  case p of
  Piece White King   -> "♔" -- U+2654	&#9812;	&#x2654;
  Piece White Queen  -> "♕" -- U+2655 &#9813;	&#x2655;
  Piece White Rook   -> "♖" -- U+2656	&#9814;	&#x2656;
  Piece White Bishop -> "♗" -- U+2657	&#9815;	&#x2657;
  Piece White Knight -> "♘" -- U+2658	&#9816;	&#x2658;
  Piece White Pawn   -> "♙" -- U+2659	&#9817;	&#x2659;
  Piece Black King   -> "♚" -- U+265A	&#9818;	&#x265A;
  Piece Black Queen  -> "♛" -- U+265B	&#9819;	&#x265B;
  Piece Black Rook   -> "♜" -- U+265C	&#9820;	&#x265C;
  Piece Black Bishop -> "♝" -- U+265D	&#9821;	&#x265D;
  Piece Black Knight -> "♞" -- U+265E	&#9822;	&#x265E;
  Piece Black Pawn   -> "♟︎" -- U+265F &#9823; &#x265F;

tileView : Board -> Int -> Int -> Maybe Piece -> Html a
tileView b i j mp =
  let whiteCheck = tileInCheck White b (j, i)
      blackCheck = tileInCheck Black b (j, i)
  in div
    [ style "position" "relative"
    , style "backgroundColor"
      <| if (modBy 2 (i + j) == 0)
          then darkSpaceColor
          else lightSpaceColor
    , style "width" checkSize
    , style "height" checkSize
    ]
    [ div
        [ style "backgroundColor" <|
          if List.isEmpty whiteCheck
          then if List.isEmpty blackCheck then "transparent" else "blue"
          else if List.isEmpty blackCheck then "red"         else "magenta"
        , style "opacity" ".2"
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
          [ text << pieceToIcon <| p ]
        )
      |> Maybe.withDefault blank
    ]
