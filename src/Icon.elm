module Icon exposing (..)

import Chess exposing (Piece(..), Player(..), PieceType(..))

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
