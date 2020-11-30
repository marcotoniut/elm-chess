module Composition exposing (..)

import Chess exposing (Position(..), Piece(..), PieceType(..), Player(..))

standardComposition : List Position
standardComposition =
  [ Position (0, 7) (Piece Black Rook)
  , Position (1, 7) (Piece Black Knight)
  , Position (2, 7) (Piece Black Bishop)
  , Position (3, 7) (Piece Black Queen)
  , Position (4, 7) (Piece Black King)
  , Position (5, 7) (Piece Black Bishop)
  , Position (6, 7) (Piece Black Knight)
  , Position (7, 7) (Piece Black Rook)
  , Position (0, 6) (Piece Black Pawn)
  , Position (1, 6) (Piece Black Pawn)
  , Position (2, 6) (Piece Black Pawn)
  , Position (3, 6) (Piece Black Pawn)
  , Position (4, 6) (Piece Black Pawn)
  , Position (5, 6) (Piece Black Pawn)
  , Position (6, 6) (Piece Black Pawn)
  , Position (7, 6) (Piece Black Pawn)
  , Position (0, 1) (Piece White Pawn)
  , Position (1, 1) (Piece White Pawn)
  , Position (2, 1) (Piece White Pawn)
  , Position (3, 1) (Piece White Pawn)
  , Position (4, 1) (Piece White Pawn)
  , Position (5, 1) (Piece White Pawn)
  , Position (6, 1) (Piece White Pawn)
  , Position (7, 1) (Piece White Pawn)
  , Position (0, 0) (Piece White Rook)
  , Position (1, 0) (Piece White Knight)
  , Position (2, 0) (Piece White Bishop)
  , Position (3, 0) (Piece White Queen)
  , Position (4, 0) (Piece White King)
  , Position (5, 0) (Piece White Bishop)
  , Position (6, 0) (Piece White Knight)
  , Position (7, 0) (Piece White Rook)
  ]


castlingComposition : List Position
castlingComposition =
  [ Position (0, 7) (Piece Black Rook)
  , Position (4, 7) (Piece Black King)
  , Position (7, 7) (Piece Black Rook)
  -- , Position (0, 6) (Piece Black Pawn)
  , Position (1, 6) (Piece Black Pawn)
  , Position (2, 6) (Piece Black Pawn)
  , Position (3, 6) (Piece Black Pawn)
  , Position (4, 6) (Piece Black Pawn)
  , Position (5, 2) (Piece Black Pawn)
  , Position (6, 6) (Piece Black Pawn)
  -- , Position (7, 6) (Piece Black Pawn)
  , Position (0, 1) (Piece White Pawn)
  , Position (1, 1) (Piece White Pawn)
  , Position (2, 4) (Piece White Pawn)
  , Position (3, 1) (Piece White Pawn)
  , Position (4, 1) (Piece White Pawn)
  , Position (5, 1) (Piece White Pawn)
  , Position (6, 1) (Piece White Pawn)
  -- , Position (7, 1) (Piece White Pawn)
  , Position (0, 0) (Piece White Rook)
  , Position (4, 0) (Piece White King)
  , Position (7, 0) (Piece White Rook)
  ]