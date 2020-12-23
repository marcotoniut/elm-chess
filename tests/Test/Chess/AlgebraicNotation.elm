module Test.Chess.AlgebraicNotation exposing (..)

import Chess exposing (..)
import Chess.AlgebraicNotation exposing (..)
import Direction exposing (..)
import Expect exposing (Expectation)
import Matrix
import Test exposing (..)

pluckTileANSuite : Test
pluckTileANSuite = describe "pluckTileAN"
  [ test "f2"
    (\_ -> Expect.equal (Just ((5, 1), "...")) <| pluckTileAN "f2...")
  ]

pluckTileReverseANSuite : Test
pluckTileReverseANSuite = describe "pluckTileReverseAN"
  [ test "e4"
    (\_ -> Expect.equal (Just ((4, 3), ""))   <| pluckTileReverseAN "4e")
  , test "f2"
    (\_ -> Expect.equal (Just ((5, 1), "...")) <| pluckTileReverseAN "2f...")
  , test "a6"
    (\_ -> Expect.equal (Just ((0, 5), ""))   <| pluckTileReverseAN "6a")
  ]

testComposition : List Tile
testComposition =
  [ Tile (0, 7) (Piece Black Rook)
  , Tile (3, 7) (Piece Black Queen)
  , Tile (5, 2) (Piece Black Pawn)
  , Tile (2, 2) (Piece White Knight)
  , Tile (4, 2) (Piece White Knight)
  , Tile (5, 5) (Piece Black Knight)
  , Tile (3, 3) (Piece White Bishop)
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
    (\_ -> Expect.equal (Result.Ok (PawnPieceMove (PawnAdvance (4, 2))))
    <| parseAN White testBoard "e4"
    )
  , test "e5"
    (\_ -> Expect.equal (Result.Ok (PawnPieceMove (PawnAdvance (4, 5))))
    <| parseAN Black testBoard "e5"
    )
  , test "O-O-O"
    (\_ -> Expect.equal (Result.Ok <| KingPieceMove <| KingCastling QueenSide)
    <| parseAN White testBoard "O-O-O"
    )
  , test "O-O"
    (\_ -> Expect.equal (Result.Ok <| KingPieceMove <| KingCastling KingSide)
    <| parseAN Black testBoard "O-O"
    )
  , test "e8Q"
    (\_ -> Expect.equal (Result.Ok (PawnPieceMove (PawnPromotion QueenPromotion (PawnPromotionAdvance 4))))
    <| parseAN White testBoard "e8Q"
    )
  , test "dxe1N"
    (\_ -> Expect.equal (Result.Ok (PawnPieceMove (PawnPromotion KnightPromotion (PawnPromotionCapture 3 Left))))
    <| parseAN White testBoard "dxe1N"
    )
  , test "Nc3d5 White"
    (\_ -> Expect.equal (Result.Ok (KnightPieceMove (KnightMove (2, 2) N Right)))
    <| parseAN White testBoard "Nc3d5"
    )
  , test "Nd5 Black"
    (\_ -> Expect.equal (Result.Ok (KnightPieceMove (KnightMove (5, 5) W Left)))
    <| parseAN Black testBoard "Nd5"
    )
  , test "Bb6"
    (\_ -> Expect.equal (Result.Ok (BishopPieceMove (BishopMove (3, 3) NW 2)))
    <| parseAN White testBoard "Bb6"
    )
  , test "a3"
    (\_ -> Expect.equal (Result.Ok (PawnPieceMove (PawnAdvance (0, 1)))) 
    <| parseAN White testBoard "a3"
    )
  ]
