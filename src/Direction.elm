module Direction exposing (..)

type alias V2 = (Int, Int)

type HorizontalDirection = Left | Right
horizontalDirection : a -> a -> HorizontalDirection -> a
horizontalDirection x y h = case h of
  Left  -> x
  Right -> y

translateHorizontal : HorizontalDirection -> Int -> Int
translateHorizontal h i = case h of
  Left  -> i - 1
  Right -> i + 1

reverseHorizontal : HorizontalDirection -> HorizontalDirection
reverseHorizontal h = case h of
   Left  -> Right
   Right -> Left

type StraightDirection = N | E | S | W
straightDirections : List StraightDirection
straightDirections = [ N, E, S, W ]
translateStraight : StraightDirection -> V2 -> V2
translateStraight d (f, r) = case d of
  N -> (f    , r + 1)
  E -> (f + 1, r    )
  S -> (f    , r - 1)
  W -> (f - 1, r    )


type DiagonalDirection = NE | SE | SW | NW
diagonalDirections : List DiagonalDirection
diagonalDirections = [ NE, SE, SW, NW ]
translateDiagonal : DiagonalDirection -> V2 -> V2
translateDiagonal d (f, r) = case d of
  NE -> (f + 1, r + 1)
  SE -> (f + 1, r - 1)
  SW -> (f - 1, r - 1)
  NW -> (f - 1, r + 1)

type Direction
  = StraightDirection StraightDirection
  | DiagonalDirection DiagonalDirection

turnDiagonal : StraightDirection -> HorizontalDirection -> DiagonalDirection
turnDiagonal d = case d of
  N -> horizontalDirection NW NE
  E -> horizontalDirection NE SE
  S -> horizontalDirection SE SW
  W -> horizontalDirection SW NW
  

directions : List Direction
directions = List.concat
  [ List.map StraightDirection straightDirections
  , List.map DiagonalDirection diagonalDirections
  ]

translate : Direction -> V2 -> V2
translate d = case d of
  StraightDirection sd -> translateStraight sd
  DiagonalDirection dd -> translateDiagonal dd
