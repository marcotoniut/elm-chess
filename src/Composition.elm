module Composition exposing (..)

import Chess exposing (Tile(..), Piece(..), PieceType(..), Player(..))

standardComposition : List Tile
standardComposition =
  [ Tile (0, 7) (Piece Black Rook)
  , Tile (1, 7) (Piece Black Knight)
  , Tile (2, 7) (Piece Black Bishop)
  , Tile (3, 7) (Piece Black Queen)
  , Tile (4, 7) (Piece Black King)
  , Tile (5, 7) (Piece Black Bishop)
  , Tile (6, 7) (Piece Black Knight)
  , Tile (7, 7) (Piece Black Rook)
  , Tile (0, 6) (Piece Black Pawn)
  , Tile (1, 6) (Piece Black Pawn)
  , Tile (2, 6) (Piece Black Pawn)
  , Tile (3, 6) (Piece Black Pawn)
  , Tile (4, 6) (Piece Black Pawn)
  , Tile (5, 6) (Piece Black Pawn)
  , Tile (6, 6) (Piece Black Pawn)
  , Tile (7, 6) (Piece Black Pawn)
  , Tile (0, 1) (Piece White Pawn)
  , Tile (1, 1) (Piece White Pawn)
  , Tile (2, 1) (Piece White Pawn)
  , Tile (3, 1) (Piece White Pawn)
  , Tile (4, 1) (Piece White Pawn)
  , Tile (5, 1) (Piece White Pawn)
  , Tile (6, 1) (Piece White Pawn)
  , Tile (7, 1) (Piece White Pawn)
  , Tile (0, 0) (Piece White Rook)
  , Tile (1, 0) (Piece White Knight)
  , Tile (2, 0) (Piece White Bishop)
  , Tile (3, 0) (Piece White Queen)
  , Tile (4, 0) (Piece White King)
  , Tile (5, 0) (Piece White Bishop)
  , Tile (6, 0) (Piece White Knight)
  , Tile (7, 0) (Piece White Rook)
  ]


castlingComposition : List Tile
castlingComposition =
  [ Tile (0, 7) (Piece Black Rook)
  , Tile (4, 7) (Piece Black King)
  , Tile (7, 7) (Piece Black Rook)
  -- , Position (0, 6) (Piece Black Pawn)
  , Tile (1, 6) (Piece Black Pawn)
  , Tile (2, 6) (Piece Black Pawn)
  , Tile (3, 6) (Piece Black Pawn)
  , Tile (4, 6) (Piece Black Pawn)
  , Tile (5, 2) (Piece Black Pawn)
  , Tile (6, 6) (Piece Black Pawn)
  -- , Position (7, 6) (Piece Black Pawn)
  , Tile (0, 1) (Piece White Pawn)
  , Tile (1, 1) (Piece White Pawn)
  , Tile (2, 4) (Piece White Pawn)
  , Tile (3, 1) (Piece White Pawn)
  , Tile (4, 1) (Piece White Pawn)
  , Tile (5, 1) (Piece White Pawn)
  , Tile (6, 1) (Piece White Pawn)
  -- , Position (7, 1) (Piece White Pawn)
  , Tile (0, 0) (Piece White Rook)
  , Tile (4, 0) (Piece White King)
  , Tile (7, 0) (Piece White Rook)
  ]