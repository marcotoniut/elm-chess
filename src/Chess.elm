module Chess exposing (..)

import Matrix
import Maybe.Extra as M
import List.Extra as L
import Result.Extra as R

type Player = Black | White
other : Player -> Player
other p = case p of
  Black -> White
  White -> Black

type PieceType = King | Queen | Rook | Bishop | Knight | Pawn
type PawnPromotion = QueenPromotion | RookPromotion | BishopPromotion | KnightPromotion

type StraightDirection = N | E | S | W
translateStraight : StraightDirection -> (Int, Int) -> (Int, Int)
translateStraight d (x, y) = case d of
  N -> (x    , y + 1)
  E -> (x + 1, y    )
  S -> (x    , y - 1)
  W -> (x - 1, y    )


type DiagonalDirection = NE | SE | SW | NW
translateDiagonal : DiagonalDirection -> (Int, Int) -> (Int, Int)
translateDiagonal d (x, y) = case d of
  NE -> (x + 1, y + 1)
  SE -> (x + 1, y - 1)
  SW -> (x - 1, y - 1)
  NW -> (x - 1, y + 1)

type Piece = Piece Player PieceType
piecePlayer : Piece -> Player
piecePlayer (Piece t _) = t
pieceType   : Piece -> PieceType
pieceType   (Piece _ t) = t

type Castling = KingSide | QueenSide
castlingRank : Player -> Int
castlingRank p = case p of
  White -> 0
  Black -> 7

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

type Rank = Int
type Tile = Tile (Int, Int) Piece

advanceTurn : { a | turn : Player } -> { a | turn : Player }
advanceTurn a = { a | turn = other a.turn }

findKing : Player -> Board -> Maybe (Int, Int)
findKing t =
  Matrix.toIndexedList
  >> L.find (Tuple.second >> M.filter (\(Piece p k) -> k == King && t == p) >> M.isJust)
  >> Maybe.map Tuple.first

-- isStraightAttacker
isStraightAttacker : PieceType -> Bool
isStraightAttacker p = case p of
  Queen -> True
  Rook  -> True
  _     -> False

straightCheck : Player -> Board -> (Int, Int) -> StraightDirection -> Maybe Tile
straightCheck t b v0 d =
  let vf = translateStraight d v0
  in Matrix.get vf b
    |> Maybe.andThen
      (\mp -> case mp of
        Nothing -> straightCheck t b vf d
        Just p  ->
          if t /= piecePlayer p && isStraightAttacker (pieceType p)
          then Just (Tile vf p)
          else Nothing
      )

straightsCheck : Player -> Board -> (Int, Int) -> List Tile
straightsCheck p b v =
  let check = straightCheck p b v
  in List.filterMap identity
  [ check N
  , check E
  , check S
  , check W
  ]


isDiagonalAttacker : PieceType -> Bool
isDiagonalAttacker p = case p of
  Queen  -> True
  Bishop -> True
  _      -> False

diagonalCheck : Player -> Board -> (Int, Int) -> DiagonalDirection -> Maybe Tile
diagonalCheck t b v0 d =
  let vf = translateDiagonal d v0
  in Matrix.get vf b
    |> Maybe.andThen
      (\mp -> case mp of
        Nothing -> diagonalCheck t b vf d
        Just p ->
          if t /= piecePlayer p && isDiagonalAttacker (pieceType p)
          then Just (Tile vf p)
          else Nothing
      )

diagonalsCheck : Player -> Board -> (Int, Int) -> List Tile
diagonalsCheck p b v =
  let check = diagonalCheck p b v
  in List.filterMap identity
  [ check NE
  , check SE
  , check SW
  , check NW
  ]


kingStraightOneCheck : Player -> Board -> (Int, Int) -> StraightDirection -> Maybe Tile
kingStraightOneCheck t b v0 d =
  let vf = translateStraight d v0
  in Matrix.get vf b
    |> M.join
    |> M.filter (\p -> t /= piecePlayer p && King == (pieceType p))
    |> Maybe.map (Tile vf)

kingDiagonalOneCheck : Player -> Board -> (Int, Int) -> DiagonalDirection -> Maybe Tile
kingDiagonalOneCheck t b v0 d =
  let vf = translateDiagonal d v0
  in Matrix.get vf b
    |> M.join
    |> M.filter (\p -> t /= piecePlayer p && King == (pieceType p))
    |> Maybe.map (Tile vf)

kingCheck : Player -> Board -> (Int, Int) -> List Tile
kingCheck p b v =
  let straightOneCheck = kingStraightOneCheck p b v
      diagonalOneCheck = kingDiagonalOneCheck p b v
  in
  [ straightOneCheck N
  , straightOneCheck E
  , straightOneCheck S
  , straightOneCheck W
  , diagonalOneCheck NE
  , diagonalOneCheck SE
  , diagonalOneCheck SW
  , diagonalOneCheck NW
  ]
  |> List.filterMap identity


knightOneCheck : Player -> Board -> (Int, Int) -> StraightDirection -> DiagonalDirection -> Maybe Tile
knightOneCheck t b v0 sD dD =
  let vf = translateStraight sD <| translateDiagonal dD v0
  in Matrix.get vf b
    |> M.join
    |> M.filter (\p -> t /= piecePlayer p && Knight == (pieceType p))
    |> Maybe.map (Tile vf)

knightCheck : Player -> Board -> (Int, Int) -> List Tile
knightCheck p b v = let check = knightOneCheck p b v in 
  [ check N NE
  , check N NW
  , check E NE
  , check E SE
  , check S SE
  , check S SW
  , check W NW
  , check W SW
  ]
  |> List.filterMap identity


pawnOneCheck : Player -> Board -> (Int, Int) -> DiagonalDirection -> Maybe Tile
pawnOneCheck t b v0 d =
  let vf = translateDiagonal d v0
  in Matrix.get vf b
    |> M.join
    |> M.filter (\p -> t /= piecePlayer p && Pawn == (pieceType p))
    |> Maybe.map (Tile vf)

pawnCheck : Player -> Board -> (Int, Int) -> List Tile
pawnCheck p b v =
  List.map (pawnOneCheck p b v) (case p of
    White -> [ NE, NW ]
    Black -> [ SE, SW ]
  )
  |> List.filterMap identity

tileInCheck : Player -> Board -> (Int, Int) -> List Tile
tileInCheck p b v = Matrix.get v b |> M.join |> M.filter (\x -> p /= piecePlayer x) |>
  (\mx -> case mx of
    Nothing ->
      [ pawnCheck p b v
      , knightCheck p b v
      , diagonalsCheck p b v
      , straightsCheck p b v
      , kingCheck p b v
      ]
      |> List.concat
    Just _  -> []
  )

type MoveError
  = CastlingUnavailable
  | LeavesPlayerInCheck Player
  | MissingPlayerKing Player
  | TrayectoryBlocked
  | InvalidTile (Int, Int)
  | NoPlayerPieceInTile
  | TileObstructed
  | MoveNotImplemented
  | ScrambledPieces

tryMove : Move -> Game -> Result MoveError Game
tryMove m s = case m of
  PieceMove v0 vf ->
    (Result.fromMaybe (InvalidTile v0) <| Matrix.get v0 s.board)
    |> Result.andThen
      (\m0 -> (Result.fromMaybe (InvalidTile vf) <| Matrix.get vf s.board)
      |> Result.andThen
        (\mf -> case m0 of
          Nothing -> Result.Err NoPlayerPieceInTile
          Just p0  ->
            let pl = piecePlayer p0 in
            case mf of
              Nothing -> s.board
                |> Matrix.set v0 Nothing
                |> Matrix.set vf (Just p0)
                |> (\b -> (b, pl))
                |> Result.Ok
              Just pf -> if piecePlayer p0 == piecePlayer pf
                then Result.Err TileObstructed
                else
                  s.board
                  |> Matrix.set v0 Nothing
                  |> Matrix.set vf (Just p0)
                  |> (\b -> (b, pl))
                  |> Result.Ok
        )
      )
    |> Result.andThen
      (\(b, pl) -> case findKing pl b of
        Nothing -> Result.Err (MissingPlayerKing pl)
        Just vk -> if List.isEmpty (tileInCheck pl b vk)
          then Result.Ok b
          else Result.Err (LeavesPlayerInCheck pl)
      )
    |> Result.map (\b -> { s | board = b })
  Castling c ->
    let p = s.turn
        rank = castlingRank p
        { available, setAvailable } = case p of
          White -> { available = s.whiteCastlingAvailable, setAvailable = \x y -> { y | whiteCastlingAvailable = x } }
          Black -> { available = s.blackCastlingAvailable, setAvailable = \x y -> { y | blackCastlingAvailable = x } }
    in case c of
      KingSide ->
        let clearFiles = List.range 5 6
            checkFiles = List.range 4 6
        in if available.kingSide
        && (clearFiles
          |> List.filterMap (\f -> Matrix.get (f, rank) s.board |> M.join)
          |> List.isEmpty
          )
        && (checkFiles
          |> List.map (\f -> tileInCheck p s.board (f, rank))
          |> List.concat
          |> List.isEmpty
          )
        then
          s.board
          |> move (4, rank) (6, rank)
          |> Maybe.andThen (move (7, rank) (5, rank))
          |> Maybe.map (\b -> { s | board = b } |> setAvailable castlingDisabled)
          |> Result.fromMaybe ScrambledPieces
        else Result.Err CastlingUnavailable
      QueenSide ->
        let clearFiles = List.range 1 3
            checkFiles = List.range 2 4
        in if available.queenSide
        && (clearFiles
          |> List.filterMap (\f -> Matrix.get (f, rank) s.board |> M.join)
          |> List.isEmpty
          )
        && (checkFiles
          |> List.map (\f -> tileInCheck p s.board (f, rank))
          |> List.concat
          |> List.isEmpty
          )
        then
          s.board
          |> move (4, rank) (2, rank)
          |> Maybe.andThen (move (0, rank) (3, rank))
          |> Maybe.map (\b -> { s | board = b } |> setAvailable castlingDisabled)
          |> Result.fromMaybe ScrambledPieces
        else Result.Err CastlingUnavailable
  PawnPromotion y0 yf p -> Result.Err MoveNotImplemented
  -- PawnPromotion y0 yf p -> Maybe.Just
  --   { s | board = s.board
  --   }

move : (Int, Int) -> (Int, Int) -> Board -> Maybe Board
move v0 vf b = Matrix.get v0 b |> Maybe.map (\p -> Matrix.set v0 Nothing b |> Matrix.set vf p)

-- accumulate : Traversable t => Applicative f => ...
play : Game -> List Move -> Result MoveError Game
play g = List.foldr (\m -> Result.andThen (Result.map advanceTurn << tryMove m)) (Result.Ok g)


toAN : Board -> Player -> Move -> Result String String
toAN b p m = case m of
  Castling c -> case c of
    KingSide  -> Result.Ok "O-O"
    QueenSide -> Result.Ok "O-O-O"
  PieceMove (x0, y0) (xf, yf) -> Result.Err ""
  PawnPromotion c0 cf t ->  Result.Err ""

-- parseAN : Board -> String -> Either String Move
