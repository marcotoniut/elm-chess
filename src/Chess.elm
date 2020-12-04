module Chess exposing (..)

import Matrix
import Maybe.Extra as M
import List.Extra as L
import Result.Extra as R
import Tuple2

type Player = White | Black
player : a -> a -> Player -> a
player w b p = case p of
  White -> w
  Black -> b
opponent : Player -> Player
opponent = player Black White

type PieceType = King | Queen | Rook | Bishop | Knight | Pawn
type PawnPromotion = QueenPromotion | RookPromotion | BishopPromotion | KnightPromotion
promote : PawnPromotion -> PieceType
promote p = case p of
  QueenPromotion  -> Queen
  RookPromotion   -> Rook
  BishopPromotion -> Bishop
  KnightPromotion -> Knight

type StraightDirection = N | E | S | W
translateStraight : StraightDirection -> (Int, Int) -> (Int, Int)
translateStraight d (f, r) = case d of
  N -> (f    , r + 1)
  E -> (f + 1, r    )
  S -> (f    , r - 1)
  W -> (f - 1, r    )


type DiagonalDirection = NE | SE | SW | NW
translateDiagonal : DiagonalDirection -> (Int, Int) -> (Int, Int)
translateDiagonal d (f, r) = case d of
  NE -> (f + 1, r + 1)
  SE -> (f + 1, r - 1)
  SW -> (f - 1, r - 1)
  NW -> (f - 1, r + 1)

type Piece = Piece Player PieceType
piecePlayer : Piece -> Player
piecePlayer (Piece p _) = p
pieceType   : Piece -> PieceType
pieceType   (Piece _ t) = t

type Castling = KingSide | QueenSide
castlingRank : Player -> Int
castlingRank = player 0 7

promotionRank : Player -> Int
promotionRank = player 7 0

pawnsRank : Player -> Int
pawnsRank = player 1 6

type Move
  = PieceMove (Int, Int) (Int, Int)
  | BishopMove (Int, Int) DiagonalDirection Int
  | Castling Castling
  | PawnPromotion Int Int PawnPromotion

type alias Board = Matrix.Matrix (Maybe Piece)

initBoard : Board
initBoard = Matrix.repeat (8, 8) Nothing

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

setBoard : { a | board : Board } -> Board -> { a | board : Board }
setBoard a x = { a | board = x }

type Rank = Int
type Tile = Tile (Int, Int) Piece

advanceTurn : { a | turn : Player } -> { a | turn : Player }
advanceTurn a = { a | turn = opponent a.turn }

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
pawnCheck p b v = p
  |> player [ NE, NW ] [ SE, SW ]
  |> List.map (pawnOneCheck p b v)
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

-- TODO All MoveError should probably have context information in Move and Game State
type MoveError
  = CastlingUnavailable
  | LeavesPlayerInCheck Player
  | MissingPlayerKing Player
  | TrayectoryBlocked
  | InvalidTile (Int, Int)
  | NoPlayerPieceInTile
  | IncorrectPawnPlayer Player -- PawnPromotion
  | TileObstructed
  | MoveNotImplemented
  | ScrambledPieces
  | InvalidPawnForward
  | InvalidPawnCapture

-- TODO Result IlegalMove (Translate (x, y) (x, y))
-- legalMove
tryMove : Move -> Game -> Result MoveError Game
tryMove m s = case m of
  BishopMove v0 d q -> Result.Err MoveNotImplemented
  PieceMove v0 vf ->
    (Result.fromMaybe (InvalidTile v0) <| Matrix.get v0 s.board)
    |> Result.andThen
      (\mp0 -> (Result.fromMaybe (InvalidTile vf) <| Matrix.get vf s.board)
      |> Result.andThen
        (\mpf -> case mp0 of
          Nothing -> Result.Err NoPlayerPieceInTile
          Just p0 ->
            let pl = piecePlayer p0
            in case mpf of
              Nothing -> s.board
                |> Matrix.set v0 Nothing
                |> Matrix.set vf (Just p0)
                |> Tuple.pair pl
                |> Result.Ok
              Just pf -> if piecePlayer p0 == piecePlayer pf
                then Result.Err TileObstructed
                else
                  s.board
                  |> Matrix.set v0 Nothing
                  |> Matrix.set vf (Just p0)
                  |> Tuple.pair pl
                  |> Result.Ok
        )
      )
    |> R.filter (LeavesPlayerInCheck s.turn) (Tuple2.uncurry isPlayerInCheck)
    |> Result.map (Tuple.second >> setBoard s)
  Castling c ->
    let pl = s.turn
        r = castlingRank pl
        { available, setAvailable } = case pl of
          White -> { available = s.whiteCastlingAvailable, setAvailable = \x y -> { y | whiteCastlingAvailable = x } }
          Black -> { available = s.blackCastlingAvailable, setAvailable = \x y -> { y | blackCastlingAvailable = x } }
    in case c of
      KingSide ->
        let clearFiles = List.range 5 6
            checkFiles = List.range 4 6
        in if available.kingSide
        && (clearFiles
          |> List.filterMap (\f -> Matrix.get (f, r) s.board |> M.join)
          |> List.isEmpty
          )
        && (checkFiles
          |> List.map (\f -> tileInCheck pl s.board (f, r))
          |> List.concat
          |> List.isEmpty
          )
        then
          s.board
          |> translate (4, r) (6, r)
          |> Maybe.andThen (translate (7, r) (5, r))
          |> Maybe.map (setBoard s >> setAvailable castlingDisabled)
          |> Result.fromMaybe ScrambledPieces
        else Result.Err CastlingUnavailable
      QueenSide ->
        let clearFiles = List.range 1 3
            checkFiles = List.range 2 4
        in if available.queenSide
        && (clearFiles
          |> List.filterMap (\f -> Matrix.get (f, r) s.board |> M.join)
          |> List.isEmpty
          )
        && (checkFiles
          |> List.map (\f -> tileInCheck pl s.board (f, r))
          |> List.concat
          |> List.isEmpty
          )
        then
          s.board
          |> translate (4, r) (2, r)
          |> Maybe.andThen (translate (0, r) (3, r))
          |> Maybe.map (setBoard s >> setAvailable castlingDisabled)
          |> Result.fromMaybe ScrambledPieces
        else Result.Err CastlingUnavailable
  PawnPromotion f0 ff pr ->
    let pl = s.turn
        pawnsR = pawnsRank <| opponent s.turn
        promotionR = promotionRank pl
        v0 = (f0, pawnsR)
        vf = (ff, promotionR)
    in (case Matrix.get v0 s.board of
      Nothing -> Result.Err (InvalidTile v0)
      Just t0 -> case t0 of
        Nothing -> Result.Err NoPlayerPieceInTile
        Just p0 ->
          let ppl = piecePlayer p0
          in if ppl /= pl
          then Result.Err (IncorrectPawnPlayer ppl)
          else case Matrix.get vf s.board of
            Nothing -> Result.Err (InvalidTile vf)
            Just tf -> case tf of
              Nothing ->
                if f0 == ff
                then
                  Matrix.set v0 Nothing s.board
                  |> Matrix.set vf (Just (Piece pl (promote pr)))
                  |> Result.Ok
                else Result.Err InvalidPawnCapture
              Just pf ->
                if f0 /= ff
                then
                  if piecePlayer pf /= pl
                  then
                    Matrix.set v0 Nothing s.board
                    |> Matrix.set vf (Just (Piece pl (promote pr)))
                    |> Result.Ok
                  else Result.Err TileObstructed
                else Result.Err InvalidPawnForward
    )
    -- After move check
    |> R.filter (LeavesPlayerInCheck pl) (isPlayerInCheck pl)
    |> Result.map (setBoard s)

isPlayerInCheck : Player -> Board -> Bool
isPlayerInCheck pl b =
  findKing pl b
  |> Maybe.map (tileInCheck pl b >> List.isEmpty)
  |> Maybe.withDefault False

translate : (Int, Int) -> (Int, Int) -> Board -> Maybe Board
translate v0 vf b = Matrix.get v0 b |> Maybe.map (\mp -> Matrix.set v0 Nothing b |> Matrix.set vf mp)

-- accumulate : Traversable t => Applicative f => ...
play : List Move -> Game -> Result MoveError Game
play ms g = List.foldr (\m -> Result.andThen (Result.map advanceTurn << tryMove m)) (Result.Ok g) ms


toAN : Board -> Player -> Move -> Result String String
toAN b p m = case m of
  Castling c -> case c of
    KingSide  -> Result.Ok "O-O"
    QueenSide -> Result.Ok "O-O-O"
  BishopMove (x0, y0) d q -> Result.Err ""
  PieceMove (x0, y0) (xf, yf) -> Result.Err ""
  PawnPromotion c0 cf t ->  Result.Err ""

-- parseAN : Board -> String -> Either String Move
