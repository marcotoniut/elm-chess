module Screen.Multiplayer exposing (..)

import Alphabet exposing (..)
import Chess.Base exposing (..)
import Component exposing (emptyAttribute)
import Direction exposing (V2)
import Html exposing (..)
import Html.Attributes exposing (..)
import List.Extra as L
import Matrix
import Maybe.Extra as M
import Result.Extra as R
import Theme exposing (..)
import View.Base exposing (..)
import View.Board exposing (..)
import View.Game exposing (..)
import View.Tile exposing (..)

type GameAction
  = BoardAction BoardAction

selectTile : V2 -> WithGame (WithPlayer (GameInputs a)) -> WithGame (WithPlayer (GameInputs a))
selectTile v m =
  let turn = gameTurn m.game
  in if turn == m.player
  then case m.maybeSelected of
    Nothing ->
      Matrix.get v m.game.board
      |> M.join
      |> M.unwrap m
        (\p  ->
          if piecePlayer p == turn
          then
            { m
            | maybeSelected
              = Just
                (v
                , List.concat
                  [ pieceLegalMoves m.game v p
                    |> List.map (Tuple.mapSecond AvailablePieceMove)
                  , pawnLegalPromotionMoves v m.game
                    |> List.map (Tuple.mapSecond AvailablePawnPromotionMove)
                  ]
                )
            }
          else m
        )
    Just (s, ms) ->
      if s == v
      then { m | maybeSelected = Nothing }
      else case L.find (\(x, _) -> x == v) ms of
        Nothing ->
          case Matrix.get v m.game.board |> M.join of
            Nothing -> { m | maybeSelected = Nothing }
            Just p  ->
              if piecePlayer p == turn
              then
                { m
                | maybeSelected
                  = Just
                    (v
                    , List.concat
                      [ pieceLegalMoves m.game v p
                        |> List.map (Tuple.mapSecond AvailablePieceMove)
                      , pawnLegalPromotionMoves v m.game
                        |> List.map (Tuple.mapSecond AvailablePawnPromotionMove)
                      ]
                    )
                }
              else { m | maybeSelected = Nothing }
        Just (_, a) ->
          case a of
            AvailablePieceMove x ->
              tryMove x m.game
              |> R.unwrap
                m
                (\g ->
                  { m
                  | game = g
                  , maybeSelected = Nothing
                  }
                )
            AvailablePawnPromotionMove x ->
              { m
              | choosingPromotion = Just <| case x of
                PawnPromotionAdvance _   -> ChoosingPromotionAdvance
                PawnPromotionCapture _ d -> ChoosingPromotionCapture d
              }
  else m

viewGame : (GameAction -> a) -> WithGame (GameInputs b) -> Html a
viewGame act m = viewBoard
  (BoardAction >> act)
  { board = m.game.board
  , choosingPromotion = m.choosingPromotion
  , maybeSelected = m.maybeSelected
  }
