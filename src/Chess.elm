module Chess exposing (..)

import Matrix
import Maybe.Extra as M
import List.Extra as L
import Result.Extra as R
import Tuple2
import List
import Matrix
import List
import Html.Attributes exposing (kind)
import Direction exposing (..)

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

type KingInCheck = KingInCheck (List Tile)

type PawnMove
  = PawnAdvance V2
  | PawnDoubleAdvance Int
  | PawnCapture HorizontalDirection
  | EnPassant Int Int


type RegularMoveError
  = OutOfBounds V2
  | TileOccupied V2
  | PlayerHasNoKing
  | IncorrectPiece PieceType

type PawnAdvanceError
  = PawnAdvanceBlocked V2 Piece
  -- PawnAdvanceMoveError
  | PawnAdvanceLeavesKingInCheck KingInCheck
  | PawnAdvanceMoveError RegularMoveError

type PawnMoveError
  = PawnAdvanceError PawnAdvanceError
  | PawnDoubleAdvanceError PawnAdvanceError

-- PawnAdvanceError ?
pawnAdvance : V2 -> Int -> Game -> Result PawnAdvanceError Game
pawnAdvance v0 i g =
  let d  = player N S g.turn
      j  = i - 1
      v1 = translateStraight d v0
  in Matrix.get v1 g.board
    |> M.unwrap
      (Result.Err (PawnAdvanceMoveError (OutOfBounds v1)))
      (M.unwrap
        (if 0 < j
        then pawnAdvance v1 j g
        else
          findKing g.turn g.board
          |> M.unwrap
            (Result.Err (PawnAdvanceMoveError PlayerHasNoKing))
            (\vk ->
              let nb  = reposition v0 v1 g.board
                  kcs = inCheck g.turn nb vk
              in if List.isEmpty kcs
              then Result.Ok
                { g | board = nb
                    , turn  = opponent g.turn
                    -- , moves = PawnAdvance v0 :: g.moves
                }
              else Result.Err (PawnAdvanceLeavesKingInCheck (KingInCheck kcs))
            )
        )
        (Result.Err << PawnAdvanceBlocked v1)
      )

pawnLegalAdvances : V2 -> Game -> List PawnMove
pawnLegalAdvances v0 g =
  R.unwrap [] (always <| List.singleton <| PawnAdvance v0)
  <| pawnAdvance v0 1 g
  -- in  [PawnAdvance v0]

pawnLegalMoves : V2 -> Game -> List PawnMove
pawnLegalMoves v g =
  [ pawnLegalAdvances v g
  ]
  |> List.concat

type RookMove
  = RookMove V2 StraightDirection Int

type BishopMove
  = BishopMove V2 DiagonalDirection Int

type KnightMove
  = KnightMove V2 StraightDirection DiagonalDirection

type QueenMove = QueenMove V2 Direction Int

type KingMove = KingMove V2 Direction
  -- TODO
  -- | Castling Castling

type KingMoveError
  = KingMoveLeavesKingInCheck KingInCheck
  | KingMoveMoveError RegularMoveError

-- PawnAdvanceError ?
-- kingMove : V2 -> Direction -> Game -> Result KingMoveError (KingMove, Game) -- head moves
kingMove : V2 -> Direction -> Game -> Result KingMoveError Game
kingMove v0 d g =
  let vf = translate d v0
  in
    Matrix.get vf g.board
    |>
      M.unwrap
      (Result.Err (KingMoveMoveError (OutOfBounds vf)))
      (\mp -> case mp of
        Nothing ->
          let nb  = reposition v0 vf g.board
              kcs = inCheck g.turn nb vf
          in if List.isEmpty kcs
          then Result.Ok
            { g | board = nb
                , turn  = opponent g.turn
                -- , moves = PawnAdvance v0 :: g.moves
            }
          else Result.Err (KingMoveLeavesKingInCheck (KingInCheck kcs))
        Just p  ->
          if piecePlayer p == g.turn
          then Result.Err (KingMoveMoveError (TileOccupied vf))
          else
            let nb  = reposition v0 vf g.board
                kcs = inCheck g.turn nb vf
            in if List.isEmpty kcs
            then Result.Ok
              { g | board = nb
                  , turn  = opponent g.turn
                  -- , moves = PawnAdvance v0 :: g.moves
              }
            else Result.Err (KingMoveLeavesKingInCheck (KingInCheck kcs))
      )
  

kingLegalMoves : V2 -> Game -> List KingMove
kingLegalMoves v0 g =
  directions
  |> List.map
    (\d ->
      kingMove v0 d g
      |> Result.toMaybe
      |> Maybe.map (always (KingMove v0 d))
    )
  |> List.filterMap identity
  

type PieceMove
  = PawnPieceMove   PawnMove
  | KnightPieceMove KnightMove
  | BishopPieceMove BishopMove
  | RookPieceMove   RookMove
  | QueenPieceMove  QueenMove
  | KingPieceMove   KingMove
  | Temp_TeleportMove V2 V2

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
  -- = PieceMove V2 V2
  = PieceMove PieceMove
  | PawnDoubleStep Int
  | Castling Castling
  -- | EnPassant Player Int HorizontalDirection
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
type Tile = Tile V2 Piece

advanceTurn : { a | turn : Player } -> { a | turn : Player }
advanceTurn a = { a | turn = opponent a.turn }

findKing : Player -> Board -> Maybe V2
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

inStraightCheck : Player -> Board -> V2 -> StraightDirection -> Maybe Tile
inStraightCheck t b v0 d =
  let vf = translateStraight d v0
  in Matrix.get vf b
    |> Maybe.andThen
      (\mp -> case mp of
        Nothing -> inStraightCheck t b vf d
        Just p  ->
          if t /= piecePlayer p && isStraightAttacker (pieceType p)
          then Just (Tile vf p)
          else Nothing
      )

inStraightsCheck : Player -> Board -> V2 -> List Tile
inStraightsCheck p b v =
  let check = inStraightCheck p b v
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

inDiagonalCheck : Player -> Board -> V2 -> DiagonalDirection -> Maybe Tile
inDiagonalCheck t b v0 d =
  let vf = translateDiagonal d v0
  in Matrix.get vf b
    |> Maybe.andThen
      (\mp -> case mp of
        Nothing -> inDiagonalCheck t b vf d
        Just p ->
          if t /= piecePlayer p && isDiagonalAttacker (pieceType p)
          then Just (Tile vf p)
          else Nothing
      )

inDiagonalsCheck : Player -> Board -> V2 -> List Tile
inDiagonalsCheck p b v =
  let check = inDiagonalCheck p b v
  in List.filterMap identity
  [ check NE
  , check SE
  , check SW
  , check NW
  ]


inKingStraightOneCheck : Player -> Board -> V2 -> StraightDirection -> Maybe Tile
inKingStraightOneCheck t b v0 d =
  let vf = translateStraight d v0
  in Matrix.get vf b
    |> M.join
    |> M.filter (\p -> t /= piecePlayer p && King == (pieceType p))
    |> Maybe.map (Tile vf)

inKingDiagonalOneCheck : Player -> Board -> V2 -> DiagonalDirection -> Maybe Tile
inKingDiagonalOneCheck t b v0 d =
  let vf = translateDiagonal d v0
  in Matrix.get vf b
    |> M.join
    |> M.filter (\p -> t /= piecePlayer p && King == (pieceType p))
    |> Maybe.map (Tile vf)

inKingCheck : Player -> Board -> V2 -> List Tile
inKingCheck p b v =
  let straightOneCheck = inKingStraightOneCheck p b v
      diagonalOneCheck = inKingDiagonalOneCheck p b v
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


inKnightOneCheck : Player -> Board -> V2 -> StraightDirection -> DiagonalDirection -> Maybe Tile
inKnightOneCheck t b v0 sD dD =
  let vf = translateStraight sD <| translateDiagonal dD v0
  in Matrix.get vf b
    |> M.join
    |> M.filter (\p -> t /= piecePlayer p && Knight == (pieceType p))
    |> Maybe.map (Tile vf)

inKnightCheck : Player -> Board -> V2 -> List Tile
inKnightCheck p b v = let check = inKnightOneCheck p b v in 
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


-- TODO Enpass Pawn ?
inPawnOneCheck : Player -> Board -> V2 -> DiagonalDirection -> Maybe Tile
inPawnOneCheck t b v0 d =
  let vf = translateDiagonal d v0
  in Matrix.get vf b
    |> M.join
    |> M.filter (\p -> t /= piecePlayer p && Pawn == (pieceType p))
    |> Maybe.map (Tile vf)

inPawnCheck : Player -> Board -> V2 -> List Tile
inPawnCheck p b v = p
  |> player [ NE, NW ] [ SE, SW ]
  |> List.map (inPawnOneCheck p b v)
  |> List.filterMap identity

inCheck : Player -> Board -> V2 -> List Tile
inCheck p b v =
  Matrix.get v b
  |> M.join
  |> M.filter (piecePlayer >> (/=) p)
  |> M.unwrap
    [ inPawnCheck
    , inKnightCheck
    , inDiagonalsCheck
    , inStraightsCheck
    , inKingCheck
    ]
    (always [])
  |> List.map (\f -> f p b v)
  |> List.concat

isPlayerInCheck : Player -> Board -> Bool
isPlayerInCheck pl b =
  findKing pl b
  |> Maybe.map (inCheck pl b >> List.isEmpty)
  |> Maybe.withDefault False

tryReposition : V2 -> V2 -> Board -> Maybe Board
tryReposition v0 vf b = Matrix.get v0 b |> Maybe.map (\mp -> Matrix.set v0 Nothing b |> Matrix.set vf mp)

reposition : V2 -> V2 -> Board -> Board
reposition v0 vf b = tryReposition v0 vf b |> Maybe.withDefault b

-- TODO All MoveError should probably have context information in Move and Game State
type MoveError
  = CastlingUnavailable
  | LeavesPlayerInCheck Player
  | MissingPlayerKing Player
  | TrayectoryBlocked
  | InvalidTile V2
  | NoPlayerPieceInTile
  | IncorrectPawnPlayer Player -- PawnPromotion
  | TileObstructed
  | MoveNotImplemented
  | ScrambledPieces
  | InvalidPawnForward
  | InvalidPawnCapture
  | PawnAdvanceTempError PawnAdvanceError
  | KingMoveTempError KingMoveError

-- type IlegalMove = LeavesPlayerInCheck

-- TODO Result IlegalMove (Translate (x, y) (x, y))
-- legalMove
tryMove : Move -> Game -> Result MoveError Game
tryMove m s =
  let pl = s.turn
  in Result.map advanceTurn <| case m of
  PawnDoubleStep f -> Result.Err ScrambledPieces

  PieceMove p -> case p of
    PawnPieceMove   pp -> case pp of
      PawnAdvance v       -> Result.mapError PawnAdvanceTempError (pawnAdvance v 1 s)
      PawnDoubleAdvance f -> Result.mapError PawnAdvanceTempError (pawnAdvance (f, pawnsRank pl) 2 s)
      PawnCapture d       -> Result.Ok s
      EnPassant f0 ff     -> Result.Ok s
    KnightPieceMove pp -> Result.Ok s
    BishopPieceMove pp -> Result.Ok s
    RookPieceMove   pp -> Result.Ok s
    QueenPieceMove  pp -> Result.Ok s
    KingPieceMove (KingMove v d) -> Result.mapError KingMoveTempError (kingMove v d s)
    Temp_TeleportMove v0 vf -> Result.Ok s


  -- PieceMove v0 vf ->
  --   (Result.fromMaybe (InvalidTile v0) <| Matrix.get v0 s.board)
  --   |> Result.andThen
  --     (\mp0 -> (Result.fromMaybe (InvalidTile vf) <| Matrix.get vf s.board)
  --     |> Result.andThen
  --       (\mpf -> case mp0 of
  --         Nothing -> Result.Err NoPlayerPieceInTile
  --         Just p0 ->
  --           let pp = piecePlayer p0
  --           in if M.filter (piecePlayer >> (==) pp) mpf |> M.isJust
  --           then Result.Err TileObstructed
  --           else s.board
  --             |> Matrix.set v0 Nothing
  --             |> Matrix.set vf (Just p0)
  --             |> Tuple.pair pp
  --             |> Result.Ok
  --       )
  --     )
  --   |> R.filter (LeavesPlayerInCheck s.turn) (Tuple2.uncurry isPlayerInCheck)
  --   |> Result.map (Tuple.second >> setBoard s)
  Castling c ->
    let r = castlingRank pl
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
          |> List.map (\f -> inCheck pl s.board (f, r))
          |> List.concat
          |> List.isEmpty
          )
        then
          s.board
          |> tryReposition (4, r) (6, r)
          |> Maybe.andThen (tryReposition (7, r) (5, r))
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
          |> List.map (\f -> inCheck pl s.board (f, r))
          |> List.concat
          |> List.isEmpty
          )
        then
          s.board
          |> tryReposition (4, r) (2, r)
          |> Maybe.andThen (tryReposition (0, r) (3, r))
          |> Maybe.map (setBoard s >> setAvailable castlingDisabled)
          |> Result.fromMaybe ScrambledPieces
        else Result.Err CastlingUnavailable
  PawnPromotion f0 ff pr ->
    let pawnsR = pawnsRank <| opponent s.turn
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

-- accumulate : Traversable t => Applicative f => ...
play : List Move -> Game -> Result MoveError Game
play ms g = List.foldr (\m -> Result.andThen (tryMove m)) (Result.Ok g) ms

checkDiagonal : Player -> Board -> V2 -> DiagonalDirection -> List (V2, Maybe Piece)
checkDiagonal pl b v0 d =
  let vf = translateDiagonal d v0
  in Matrix.get vf b
    |> M.unwrap
      []
      (\mp -> M.unwrap
        ((vf, mp) :: checkDiagonal pl b vf d)
        (\p -> if pl == piecePlayer p then [] else [(vf, mp)])
        mp
      )

checkStraight : Player -> Board -> V2 -> StraightDirection -> List (V2, Maybe Piece)
checkStraight pl b v0 d =
  let vf = translateStraight d v0
  in Matrix.get vf b
    |> M.unwrap
      []
      (\mp -> M.unwrap
        ((vf, mp) :: checkStraight pl b vf d)
        (\p -> if pl == piecePlayer p then [] else [(vf, mp)])
        mp
      )

-- temp_kingLegalMoves : Player -> Game -> V2 -> List Move
-- temp_kingLegalMoves pl g v =
--   let mt = Matrix.get v g.board
--   in
--     straightDirections
--     |> List.map
--       ((\d -> translateStraight d v) >> (PieceMove v))
--     |> List.filter (\m -> tryMove m g |> R.isOk)

-- TODO
-- PieceMove
pieceLegalMoves : Game -> V2 -> Piece -> List PieceMove
pieceLegalMoves g v (Piece pl p) = case p of
  King -> List.map KingPieceMove (kingLegalMoves v g)
  _    -> []
--   Queen -> []
--   Bishop -> []
--   Queen -> []
--   Queen -> []


playerPieces : Player -> Board -> List (V2, Piece)
playerPieces pl
  = Matrix.toIndexedList
  >> List.filterMap (\(v, mp) -> M.filter (piecePlayer >> (==) pl) mp |> Maybe.map (Tuple.pair v))

type PlayerStatus
  = Invalid
  | Normal
  | Check
  | CheckMate
  | StaleMate

-- TODO canMoveOut, captureAttacker, canBlock
-- { board : Board, lastMove : lastMove } Empass
playerStatus : Player -> Game -> PlayerStatus
playerStatus pl g =
  let mk = findKing pl g.board
  in case mk of
    Nothing -> Invalid
    Just vk ->
      let kInCheck = inCheck pl g.board vk
      in case kInCheck of
        [] -> Normal
        -- One vs More
        h :: xs ->
          let ps = playerPieces pl g.board
              ms = List.concat <| List.map (Tuple2.uncurry <| pieceLegalMoves g) ps
          in if List.isEmpty ms
          then StaleMate
          else
            let klms = kingLegalMoves vk g
            in if List.isEmpty klms -- TODO other ways of escaping Checkmate
            then CheckMate
            else Check

checkMatrix : V2 -> Board -> Matrix.Matrix Bool
checkMatrix v b =
  let empty = (Matrix.repeat (Matrix.size b) False)
  in Matrix.get v b
  |> M.join
  |> M.unwrap
    (Matrix.repeat (Matrix.size b) False)
    (\(Piece pl t) -> case t of
      Bishop -> diagonalDirections
        |> List.map (checkDiagonal pl b v)
        |> List.concat
        |> List.foldl (\(v1, _) -> Matrix.set v1 True) empty
      Rook -> straightDirections
        |> List.map (checkStraight pl b v)
        |> List.concat
        |> List.foldl (\(v1, _) -> Matrix.set v1 True) empty
      Queen ->
        [ List.map (checkStraight pl b v) straightDirections
        , List.map (checkDiagonal pl b v) diagonalDirections
        ]
        |> List.concat
        |> List.concat
        |> List.foldl (\(v1, _) -> Matrix.set v1 True) empty
      -- TODO Rest
      --  { ne = checkDiagonal pl NE b v
      --  , se = checkDiagonal pl SE b v
      --  , sw = checkDiagonal pl SW b v
      --  , nw = checkDiagonal pl NW b v
      --  }
      --  Rook   -> inStraightsCheck pl b v
      _      -> Matrix.repeat (Matrix.size b) False
    )
