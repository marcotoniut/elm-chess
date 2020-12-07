module Direction exposing (..)

type HorizontalDirection = Left | Right

type StraightDirection = N | E | S | W
straightDirections : List StraightDirection
straightDirections = [ N, E, S, W ]
translateStraight : StraightDirection -> (Int, Int) -> (Int, Int)
translateStraight d (f, r) = case d of
  N -> (f    , r + 1)
  E -> (f + 1, r    )
  S -> (f    , r - 1)
  W -> (f - 1, r    )


type DiagonalDirection = NE | SE | SW | NW
diagonalDirections : List DiagonalDirection
diagonalDirections = [ NE, SE, SW, NW ]
translateDiagonal : DiagonalDirection -> (Int, Int) -> (Int, Int)
translateDiagonal d (f, r) = case d of
  NE -> (f + 1, r + 1)
  SE -> (f + 1, r - 1)
  SW -> (f - 1, r - 1)
  NW -> (f - 1, r + 1)
