module Icon exposing (..)

import Chess.Base exposing (Piece(..), PieceType(..), Player(..))

pieceToIcon : Piece -> String
pieceToIcon p =  case p of
  Piece White King   -> "Chess_klt45.svg"
  Piece White Queen  -> "Chess_qlt45.svg"
  Piece White Rook   -> "Chess_rlt45.svg"
  Piece White Bishop -> "Chess_blt45.svg"
  Piece White Knight -> "Chess_nlt45.svg"
  Piece White Pawn   -> "Chess_plt45.svg"
  Piece Black King   -> "Chess_kdt45.svg"
  Piece Black Queen  -> "Chess_qdt45.svg"
  Piece Black Rook   -> "Chess_rdt45.svg"
  Piece Black Bishop -> "Chess_bdt45.svg"
  Piece Black Knight -> "Chess_ndt45.svg"
  Piece Black Pawn   -> "Chess_pdt45.svg"
