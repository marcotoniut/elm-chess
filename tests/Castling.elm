module Castling exposing (..)

import Chess exposing (..)
import Expect exposing (Expectation)
import Test exposing (..)

suite : Test
suite =
  let composition =
        [ Tile (0, 7) (Piece Black Rook)
        , Tile (0, 3) (Piece Black Bishop)
        , Tile (3, 7) (Piece Black Queen)
        , Tile (1, 3) (Piece Black Bishop)
        , Tile (7, 7) (Piece Black Rook)
        , Tile (5, 2) (Piece Black Pawn)
        , Tile (0, 1) (Piece White Pawn)
        , Tile (1, 1) (Piece White Pawn)
        , Tile (2, 1) (Piece White Pawn)
        , Tile (3, 1) (Piece White Pawn)
        , Tile (4, 1) (Piece White Pawn)
        , Tile (5, 1) (Piece White Pawn)
        , Tile (6, 1) (Piece White Pawn)
        , Tile (0, 0) (Piece White Rook)
        , Tile (4, 0) (Piece White King)
        , Tile (7, 0) (Piece White Rook)
        ]
  in test "castling"
    (\_ -> tryMove composition |> Expect.equal Result.Err)
