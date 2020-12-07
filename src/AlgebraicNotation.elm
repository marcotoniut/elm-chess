module AlgebraicNotation exposing (..)

import Chess exposing (..)

-- TODO
toAN : Board -> Player -> Move -> Result String String
toAN b p m = case m of
  Castling c -> case c of
    KingSide  -> Result.Ok "O-O"
    QueenSide -> Result.Ok "O-O-O"
  -- BishopMove (x0, y0) d q -> Result.Err ""
  PieceMove (x0, y0) (xf, yf) -> Result.Err ""
  PawnPromotion c0 cf t ->  Result.Err ""
  PawnDoubleStep i -> Result.Ok ""

-- parseAN : Board -> String -> Either String Move