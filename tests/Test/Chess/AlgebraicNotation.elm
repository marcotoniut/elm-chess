module Test.Chess.AlgebraicNotation exposing (..)

import Chess exposing (..)
import Chess.AlgebraicNotation exposing (..)
import Expect exposing (Expectation)
import Matrix
import Test exposing (..)

pluckTileReverseANSuite : Test
pluckTileReverseANSuite = describe "pluckTileReverseAN"
  [ test "e4"
    (\_ -> Expect.equal (Just ((4, 4), ""))   <| pluckTileReverseAN "4e")
  , test "e5"
    (\_ -> Expect.equal (Just ((5, 2), "xx")) <| pluckTileReverseAN "2fxx")
  , test "a6"
    (\_ -> Expect.equal (Just ((0, 6), ""))   <| pluckTileReverseAN "6a")
  ]

testComposition : List Tile
testComposition =
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

testBoard : Board
testBoard = List.foldl
  (\(Tile (x, y) p) g -> Matrix.set (x, y) (Just p) g)
  initBoard
  testComposition

parseANSuite : Test
parseANSuite = describe "parseAN"
  [ test "e4"
    (\_ -> Expect.equal (Result.Ok (PawnPieceMove (PawnAdvance (4, 3))))
      <| parseAN White testBoard "e4"
    )
  , test "e5"
    (\_ -> Expect.equal (Result.Ok (PawnPieceMove (PawnAdvance (4, 4))))
      <| parseAN White testBoard "e5"
    )
  -- , test "Nf3"
  --   (\_ -> Expect.equal (Result.Ok (KnightPieceMove (KnightMove (4, 3))))
  --     <| parseAN White testBoard "Nf3"
  --   )
  -- , test "Nc6"
  --   (\_ -> Expect.equal (Result.Ok (KnightPieceMove (KnightMove (4, 3))))
  --     <| parseAN White testBoard "Nc6"
  --   )
  -- , test "Bb5"
  --   (\_ -> Expect.equal (Result.Ok (BishopPieceMove (BishopMove (4, 3))))
  --     <| parseAN White testBoard "Bb5"
  --   )
  , test "a6"
    (\_ -> Expect.equal (Result.Ok (PawnPieceMove (PawnAdvance (4, 3)))) 
      <| parseAN White testBoard "a6"
    )
  ]
