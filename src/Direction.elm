module Direction exposing (..)

type alias V2 = (Int, Int)

type Translation = Translation V2 V2
origin : Translation -> V2
origin (Translation o _) = o

destination : Translation -> V2
destination (Translation _ d) = d

type HorizontalDirection = Left | Right
horizontalDirection : a -> a -> HorizontalDirection -> a
horizontalDirection x y h = case h of
  Left  -> x
  Right -> y

translateHorizontal : HorizontalDirection -> Int -> Int
translateHorizontal h i = i + horizontalDirection -1 1 h

reverseHorizontal : HorizontalDirection -> HorizontalDirection
reverseHorizontal = horizontalDirection Right Left

horizontalToStraight : HorizontalDirection -> StraightDirection
horizontalToStraight = horizontalDirection W E

type StraightDirection = N | E | S | W
straightDirections : List StraightDirection
straightDirections = [ N, E, S, W ]
translateStraight : StraightDirection -> V2 -> V2
translateStraight d (f, r) = case d of
  N -> (f    , r + 1)
  E -> (f + 1, r    )
  S -> (f    , r - 1)
  W -> (f - 1, r    )

oppositeStraight : StraightDirection -> StraightDirection
oppositeStraight d = case d of
  N -> S
  S -> N
  W -> E
  E -> W

type DiagonalDirection = NE | SE | SW | NW
diagonalDirections : List DiagonalDirection
diagonalDirections = [ NE, SE, SW, NW ]
translateDiagonal : DiagonalDirection -> V2 -> V2
translateDiagonal d (f, r) = case d of
  NE -> (f + 1, r + 1)
  SE -> (f + 1, r - 1)
  SW -> (f - 1, r - 1)
  NW -> (f - 1, r + 1)

oppositeDiagonal : DiagonalDirection -> DiagonalDirection
oppositeDiagonal d = case d of
  NE -> SW
  SE -> NW
  NW -> SE
  SW -> NW

type Direction
  = StraightDirection StraightDirection
  | DiagonalDirection DiagonalDirection

opposite : Direction -> Direction
opposite d = case d of
  StraightDirection sd -> StraightDirection <| oppositeStraight sd
  DiagonalDirection dd -> DiagonalDirection <| oppositeDiagonal dd

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

straightDiff : V2 -> V2 -> Maybe (StraightDirection, Int)
straightDiff (x0, y0) (x1, y1) =
  let dx = x1 - x0
      dy = y1 - y0
  in if dx == 0 && dy /= 0
  then Just <| if dy < 0
    then (S, Basics.abs dy)
    else (N, Basics.abs dy)
  else if dy == 0 && dx /= 0
  then Just <| if dx < 0
    then (W, Basics.abs dx)
    else (E, Basics.abs dx)
  else Nothing

diagonalDiff : V2 -> V2 -> Maybe (DiagonalDirection, Int)
diagonalDiff (x0, y0) (x1, y1) =
  let dx = x1 - x0
      dy = y1 - y0
      n  = Basics.abs dx
  in if n == 0 || n /= Basics.abs dy
  then Nothing
  else Just <|
    if dx < 0
    then
      if dy < 0
      then (SW, n)
      else (NE, n)
    else
      if dy < 0
      then (NW, n)
      else (SE, n)
