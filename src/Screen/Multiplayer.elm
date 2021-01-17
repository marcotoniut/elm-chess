module Screen.Multiplayer exposing (..)

import Chess.Base exposing (..)
import Direction exposing (V2)
import Html exposing (..)
import List.Extra as L
import Matrix
import Maybe.Extra as M
import Result.Extra as R
import View.Base exposing (..)
import View.Board exposing (..)

type GameAction
  = BoardAction BoardAction

selectTile : V2 -> HasGame (HasPlayer (GameInputs a)) -> HasGame (HasPlayer (GameInputs a))
selectTile v m =
  let turn = gameTurn m.game
  in if turn /= m.player
  then m
  else case m.maybeSelected of
    Nothing ->
      Matrix.get v m.game.board
      |> M.join
      |> M.filter (\p -> piecePlayer p == turn)
      |> M.unwrap
        m
        (\p  ->
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
        )
    Just (s, ms) ->
      if s == v
      then { m | maybeSelected = Nothing }
      else case L.find (\(x, _) -> x == v) ms of
        Nothing ->
          Matrix.get v m.game.board
          |> M.join
          |> M.filter (\p -> piecePlayer p == turn)
          |> M.unwrap
            ({ m | maybeSelected = Nothing })
            (\p ->
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
            )
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

gameView : Player -> (GameAction -> a) -> HasGame (GameInputs b) -> Html a
gameView pl act m = boardView
  pl
  (pl == gameTurn m.game)
  (BoardAction >> act)
  { board = m.game.board
  , choosingPromotion = m.choosingPromotion
  , maybeSelected = m.maybeSelected
  }
