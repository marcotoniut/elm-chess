module Chess exposing (..)

import Matrix

type Player = Black | White
other : Player -> Player
other p = case p of
  Black -> White
  White -> Black

type PieceType = King | Queen | Rook | Bishop | Knight | Pawn
type PawnPromotion = QueenPromotion | RookPromotion | BishopPromotion | KnightPromotion

type StraightDirection = N | E | S | W
type DiagonalDirection = NE | SE | SW | NW

type Piece = Piece Player PieceType
piecePlayer : Piece -> Player
piecePlayer (Piece t _) = t
pieceType   : Piece -> PieceType
pieceType   (Piece _ t) = t

type Castling = KingSide | QueenSide
type Move
  = PieceMove (Int, Int) (Int, Int)
  | Castling Castling
  | PawnPromotion Int Int PawnPromotion

type alias Board = Matrix.Matrix (Maybe Piece)

initBoard : Board
initBoard = Matrix.initialize (8, 8) (always Nothing)

type alias CastlingAvailable =
  { kingSide  : Bool
  , queenSide : Bool
  }

castlingEnabled : CastlingAvailable
castlingEnabled =
  { kingSide  = True
  , queenSide = True
  }

castlingDisabled : CastlingAvailable
castlingDisabled =
  { kingSide  = False
  , queenSide = False
  }

type alias Game =
  { board : Board
  , blackCastlingAvailable : CastlingAvailable
  , whiteCastlingAvailable : CastlingAvailable
  , turn : Player
  }

type Position = Position (Int, Int) Piece

toAN : Board -> Player -> Move -> Result String String
toAN b p m = case m of
  Castling c -> case c of
    KingSide  -> Result.Ok "O-O"
    QueenSide -> Result.Ok "O-O-O"
  PieceMove (x0, y0) (xf, yf) -> Result.Err ""
  PawnPromotion c0 cf t ->  Result.Err ""

-- parseAN : Board -> String -> Either String Move

advanceTurn : { a | turn : Player } -> { a | turn : Player }
advanceTurn a = { a | turn = other a.turn }

diagonalAttacker : PieceType -> Bool
diagonalAttacker p = case p of
  Queen  -> True
  Bishop -> True
  _      -> False

straightAttacker : PieceType -> Bool
straightAttacker p = case p of
  Queen -> True
  Rook  -> True
  _     -> False

diagonalCheck : Player -> DiagonalDirection -> Board -> (Int, Int) -> Maybe Position
diagonalCheck t d b (x0, y0) =
  let (x1, y1) = case d of
        NE -> (x0 + 1, y0 + 1)
        SE -> (x0 + 1, y0 - 1)
        SW -> (x0 - 1, y0 - 1)
        NW -> (x0 - 1, y0 + 1)
  in Matrix.get (x1, y1) b
    |> Maybe.andThen
      (\mp -> case mp of
        Nothing -> diagonalCheck t d b (x1, y1)
        Just p -> if t /= piecePlayer p && diagonalAttacker (pieceType p) then Just (Position (x1, y1) p) else Nothing
      )

-- knightCheck : Player -> DiagonalDirection -> Board -> (Int, Int) -> Maybe Position
-- knightCheck p d b (x0, y0) =
--   let (x1, y1) = case d of
--         NE -> (x0 + 1, y0 + 1)
--         SE -> (x0 + 1, y0 - 1)
--         SW -> (x0 - 1, y0 - 1)
--         NW -> (x0 - 1, y0 + 1)
--   in Matrix.get (x1, y1) b
--     |> Maybe.andThen
--       (\mp -> case mp of
--         Nothing    -> diagonalCheck p d b (x1, y1)
--         Just piece -> if p /= piece.player && diagonalAttacker piece.pieceType then Just (Position x1 y1 piece) else Nothing
      -- )

-- tileInCheck : Player -> Board -> (Int, Int) -> List Position
-- tileInCheck p b = 

maybeMove : Move -> Game -> Maybe Game
maybeMove m s = case m of
  PieceMove (x0, y0) (xf, yf) -> 
    Just s
    -- Maybe.map2
    --   (\m0 mf -> case m0 of
    --     Nothing -> Nothing
    --     Just p  -> Just
    --       { s | board = s.board
    --       }
    --   )
    --   (Matrix.get (x0, y0) s.board) (Matrix.get (xf, yf) s.board)
  Castling c ->
    let row = castlingRow s.turn
        castlingAvailble = case s.turn of
          White -> { state = s.whiteCastlingAvailable, set = \x y -> { y | whiteCastlingAvailable = x } }
          Black -> { state = s.blackCastlingAvailable, set = \x y -> { y | blackCastlingAvailable = x } }
    in case c of
      KingSide -> if castlingAvailble.state.kingSide
        then
          { s | board = s.board }
          |> castlingAvailble.set castlingDisabled
          |> Maybe.Just
        else Nothing
      QueenSide -> if castlingAvailble.state.queenSide
        then 
          { s | board = s.board } 
          |> castlingAvailble.set castlingDisabled
          |> Maybe.Just
        else Nothing 
  PawnPromotion y0 yf p -> Maybe.Just
    { s | board = s.board
    }

castlingRow : Player -> Int
castlingRow p = case p of
  White -> 0
  Black -> 7

-- accumulate : Traversable t => Applicative f => ...
play : Maybe Game -> List Move -> Maybe Game
play = List.foldr (\m -> Maybe.andThen (maybeMove m << advanceTurn))



-- threatening : Board -> (Int, Int) -> List Position
-- threatening b =