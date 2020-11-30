module Main exposing (..)

import Alphabet exposing (intToAlphabet)
import Array
import Browser
import Chess exposing (
    Board, Castling(..), Game, Move(..), Piece(..), PieceType(..), Player(..), Position(..)
  , initBoard, castlingEnabled, other, play, tileInCheck
  )
import Component exposing (blank)
import Composition exposing (standardComposition, castlingComposition)
import Debug
import Html.Attributes exposing (width, height, style)
import Html exposing (Html, button, node, div, ul, li, span, text, input)
import Html.Events exposing (onInput, onClick)
import Matrix

-- type CardinalDirection = N | E | S | W
-- type IntercardinalDirection = NE | SE | SW | NW

-- type alias Direction = CardinalDirection | IntercardinalDirection

-- MODEL
type alias Model =
  { board : Board
  , input : String
  , moves: List Move
  , turn : Player
  }

init : Model
init =
  { board = List.foldl
    (\(Position (x, y) p) g -> Matrix.set (x, y) (Just p) g)
    initBoard
    -- standardComposition
    castlingComposition
  , input = ""
  , moves =
    [ Castling KingSide
    , Castling QueenSide
    -- , PieceMove (2, 3) (4, 3)
    -- , PieceMove (1, 4) (2, 4)
    -- , PieceMove (2, 2) (6, 2)
    -- , PieceMove (2, 3) (1, 3)
    ]
  , turn = White
  }

-- MAIN
main =
  Browser.sandbox { init = init, update = update, view = view }

-- UPDATE
type Msg
  = MovePiece Move
  | ChangeInput String

update : Msg -> Model -> Model
update msg model = case msg of
    ChangeInput i -> { model | input = i }
    MovePiece m ->
      { model | moves = m :: model.moves
              , turn = other model.turn
      }

darkSpaceColor = "#769656" -- (118,150,86)
lightSpaceColor = "#eeeed2" -- (238,238,210)
borderColor = "#baca44" -- (186,202,68)
whitePlayerColor = "#ffffff" -- (255,255,255)
blackPlayerColor = "#000000" -- (0,0,0)
checkSize = "70px"

-- VIEW
view : Model -> Html Msg
view model =
  div []
    [ button [ onClick (MovePiece (Castling KingSide)) ] [ text "-" ]
    , button [ onClick (MovePiece (Castling QueenSide)) ] [ text "+" ]
    , input [ onInput ChangeInput ] []
    , div
      []
      [ model.moves
        |> List.indexedMap
          (\i m ->
            case m of
              PieceMove (x0, y0) (xf, yf) ->
                li
                [ style "backgroundColor" <| if modBy 2 i == 0 then "white" else "lightgrey" ]
                [ text
                  <| "[" ++ Debug.toString x0 ++ " " ++ Debug.toString y0
                  ++ " - " ++ Debug.toString xf ++ " " ++ Debug.toString yf
                  ++ "]"
                ]
              Castling c ->
                li
                [ style "backgroundColor" <| if modBy 2 i == 0 then "white" else "lightgrey" ]
                [ text <| Debug.toString c ]
              PawnPromotion y0 yf p ->
                li
                [ style "backgroundColor" <| if modBy 2 i == 0 then "white" else "lightgrey" ]
                [ text
                  <| "[" ++ Debug.toString y0 ++ " " ++ Debug.toString yf
                  ++ " -> " ++ Debug.toString p
                  ++ "]"
                ]
          )
        |> ul []
      ]
    , div
      [ style "display" "flex"
      ]
      [ play (Just
          { board = model.board
          , blackCastlingAvailable = castlingEnabled
          , whiteCastlingAvailable = castlingEnabled
          , turn = White
          }) model.moves
        |> Maybe.map (Matrix.toList << .board)
        -- |> Maybe.withDefault (\e -> div [] [ text "ERROR" ])
        -- TODO Error
        |> Maybe.withDefault (Matrix.toList model.board)
        -- |> Maybe.withDefault []
        |> List.indexedMap
            (\i xs -> div
              [ style "display" "flex"
              ]
              (List.indexedMap (\j mp ->
                div
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
                        if List.isEmpty (tileInCheck White model.board (j, i))
                        then if List.isEmpty (tileInCheck Black model.board (j, i))
                          then "transparent"
                          else "blue"
                        else if List.isEmpty (tileInCheck Black model.board (j, i))
                          then "red"
                          else "magenta"
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
