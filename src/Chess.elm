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

type RegularMoveError
  = OutOfBounds V2
  | PathBlocked V2
  | PlayerHasNoKing
  | IncorrectPiece PieceType

type PawnMove
  = PawnAdvance V2
  | PawnDoubleAdvance Int
  | PawnCapture V2 HorizontalDirection
  | PawnEnPassant HorizontalDirection
  | PawnPromotion Int Int PawnPromotion

type PawnAdvanceError
  = PawnAdvanceBlocked V2 Piece
  | PawnAdvanceLeavesKingInCheck KingInCheck
  | PawnAdvanceMoveError RegularMoveError

pawnAdvanceForward : V2 -> Int -> Game -> Result PawnAdvanceError V2
pawnAdvanceForward v0 i g =
  let pl = gameTurn g
      v1 = translateStraight (player N S pl) v0
  in Matrix.get v1 g.board
    |> M.unwrap
      (Result.Err (PawnAdvanceMoveError (OutOfBounds v1)))
      (\mp -> case mp of
        Nothing ->
          let j  = i - 1
          in if 0 < j
          then pawnAdvanceForward v1 j g
          else Result.Ok v1
        Just p -> Result.Err <| PawnAdvanceBlocked v1 p
      )

-- PawnAdvanceError ?
pawnAdvance : V2 -> Int -> Game -> Result PawnAdvanceError Board
pawnAdvance v0 i g =
  let pl = gameTurn g
  in pawnAdvanceForward v0 i g
    |> Result.andThen
    (\vf ->
      findKing pl g.board
      |> M.unwrap
        (Result.Err (PawnAdvanceMoveError PlayerHasNoKing))
        (\vk ->
          let nb  = reposition v0 vf g.board
              kcs = inCheck pl nb vk
          in if List.isEmpty kcs
          then Result.Ok nb
          else Result.Err (PawnAdvanceLeavesKingInCheck (KingInCheck kcs))
        )
    )

pawnLegalAdvances : V2 -> Game -> List PawnMove
pawnLegalAdvances v0 g =
  R.unwrap [] (always <| List.singleton <| PawnAdvance v0)
  <| pawnAdvance v0 1 g

pawnLegalDoubleAdvances : Int -> Game -> List PawnMove
pawnLegalDoubleAdvances f g =
  R.unwrap [] (always <| List.singleton <| PawnDoubleAdvance f)
  <| pawnAdvance (f, pawnsRank (gameTurn g)) 2 g


type PawnCaptureError
  = PawnCaptureNoTarget HorizontalDirection
  | PawnCaptureLeavesKingInCheck KingInCheck
  | PawnCaptureMoveError RegularMoveError

pawnCapture : V2 -> HorizontalDirection -> Game -> Result PawnCaptureError Board
pawnCapture v0 h g =
  let pl = gameTurn g
      d  = pl |> case h of
            Left  -> player NW SW
            Right -> player NE SE
      vf = translateDiagonal d v0
  in Matrix.get vf g.board
    |> M.unwrap
      (Result.Err (PawnCaptureMoveError (OutOfBounds vf)))
      (\mp -> case mp of
        Nothing -> Result.Err <| PawnCaptureNoTarget h
        Just p  ->
          findKing pl g.board
          |> M.unwrap
            (Result.Err (PawnCaptureMoveError PlayerHasNoKing))
            (\vk ->
              let nb  = reposition v0 vf g.board
                  kcs = inCheck pl nb vk
              in if List.isEmpty kcs
              then Result.Ok nb
              else Result.Err (PawnCaptureLeavesKingInCheck (KingInCheck kcs))
            )
      )

pawnLegalCaptures : V2 -> Game -> List PawnMove
pawnLegalCaptures v0 g =
  [ Left, Right ]
  |> List.filterMap
    (\d ->
      pawnCapture v0 d g
      |> Result.toMaybe
      |> Maybe.map (always (PawnCapture v0 d))
    )


type PawnEnPassantError
  = PawnEnPassantUnavailable
  | PawnEnPassantNoAttacker V2
  | PawnEnPassantLeavesKingInCheck KingInCheck
  | PawnEnPassantMoveError RegularMoveError

pawnEnPassant : HorizontalDirection -> Game -> Result PawnEnPassantError Board
pawnEnPassant h g =
  let pl = gameTurn g
      ml = List.head g.moves
      b  = g.board
  in case ml of
    Nothing -> Result.Err PawnEnPassantUnavailable
    Just  l -> case l of
      PawnPieceMove (PawnDoubleAdvance f) ->
        let v0 = (translateHorizontal (reverseHorizontal h) f, enPassantRank pl)
        in Matrix.get v0 b
          -- TODO filter
          |> M.join
          |> M.unwrap
            (Result.Err (PawnEnPassantNoAttacker v0))
            (\p ->
              if piecePlayer p /= pl
              then Result.Err (PawnEnPassantNoAttacker v0)
              else case pieceType p of
                Pawn ->
                  findKing pl b
                  |> M.unwrap
                    (Result.Err (PawnEnPassantMoveError PlayerHasNoKing))
                    (\vk ->
                      let d   = pl |> case h of
                            Left  -> player NW SW
                            Right -> player NE SE
                          nb  = reposition v0 (translateDiagonal d v0) b
                          kcs = inCheck pl nb vk
                      in if List.isEmpty kcs
                      then Result.Ok nb
                      else Result.Err (PawnEnPassantLeavesKingInCheck (KingInCheck kcs))
                    )
                _ -> Result.Err (PawnEnPassantNoAttacker v0)
            )
      _ -> Result.Err PawnEnPassantUnavailable

pawnLegalEnPassants : Game -> List PawnMove
pawnLegalEnPassants g =
  [ Left, Right ]
  |> List.filterMap
    (\d ->
      pawnEnPassant d g
      |> Result.toMaybe
      |> Maybe.map (always (PawnEnPassant d))
    )

type PawnPromotionError
  = PawnPromotionBlocked Int Piece
  | PawnPromotionCaptureNoTarget HorizontalDirection
  | PawnPromotionLeavesKingInCheck KingInCheck
  | PawnPromotionMoveError RegularMoveError

pawnPromotion : Int -> Int -> PawnPromotion -> Game -> Result PawnPromotionError Board
pawnPromotion f0 ff pr g =
    let pl = gameTurn g
        b  = g.board
        pawnsR = pawnsRank <| opponent pl
        promotionR = promotionRank pl
        v0 = (f0, pawnsR)
        vf = (ff, promotionR)
    in case Matrix.get vf b of
      Nothing -> Result.Err (PawnPromotionMoveError (OutOfBounds vf))
      -- TODO
      -- let nb  = reposition v0 vf g.board
      --     kcs = inCheck g.turn nb vk
      -- in if List.isEmpty kcs
      -- then Result.Ok
      --   { g | board = nb
      --       , turn  = opponent g.turn
      --       -- , moves = PawnAdvance v0 :: g.moves
      --   }
      -- else Result.Err (PawnCaptureLeavesKingInCheck (KingInCheck kcs))
      Just tf -> case tf of
        Nothing ->
          if f0 == ff
          then
            Matrix.set v0 Nothing b
            |> Matrix.set vf (Just (Piece pl (promote pr)))
            |> Result.Ok
          -- TODO
          -- else Result.Err InvalidPawnCapture
          else Result.Ok b
        Just pf ->
          if f0 /= ff
          then
            if piecePlayer pf /= pl
            then
              Matrix.set v0 Nothing b
              |> Matrix.set vf (Just (Piece pl (promote pr)))
              |> Result.Ok
            else Result.Err (PawnPromotionMoveError (PathBlocked vf))
          -- TODO
          -- else Result.Err (PawnPromotionMoveError InvalidPawnForward)
          else Result.Ok b


pawnLegalMoves : V2 -> Game -> List PawnMove
pawnLegalMoves v g =
  [ pawnLegalAdvances v g
  -- TODO
  -- , pawnLegalDoubleAdvances v g
  , pawnLegalCaptures v g
  ]
  |> List.concat


type KnightMove
  = KnightMove V2 StraightDirection DiagonalDirection

type KnightMoveError
  = KnightMoveLeavesKingInCheck KingInCheck
  | KnightMoveMoveError RegularMoveError

knightMove : V2 -> StraightDirection -> DiagonalDirection -> Game -> Result KnightMoveError Board
knightMove v0 sd dd g =
  let vf = translateDiagonal dd <| translateStraight sd v0
      pl = gameTurn g
  in
    Matrix.get vf g.board
    |> M.unwrap
      (Result.Err (KnightMoveMoveError (OutOfBounds vf)))
      (\mp -> case mp of
        Nothing ->
          let nb  = reposition v0 vf g.board
              kcs = inCheck pl nb vf
          in if List.isEmpty kcs
          then Result.Ok nb
          else Result.Err (KnightMoveLeavesKingInCheck (KingInCheck kcs))
        Just p  ->
          if piecePlayer p == pl
          then Result.Err (KnightMoveMoveError (PathBlocked vf))
          else
            let nb  = reposition v0 vf g.board
                kcs = inCheck pl nb vf
            in if List.isEmpty kcs
            then Result.Ok nb
            else Result.Err (KnightMoveLeavesKingInCheck (KingInCheck kcs))
      )


type BishopMove
  = BishopMove V2 DiagonalDirection Int

type BishopMoveError
  = BishopMoveLeavesKingInCheck KingInCheck
  | BishopBlocked DiagonalDirection V2
  | BishopMoveMoveError RegularMoveError

bishopMoveForward : V2 -> DiagonalDirection -> Int -> Game -> Result BishopMoveError V2
bishopMoveForward v0 d i g =
  let pl = gameTurn g
      v1 = translateDiagonal d v0
      j  = i - 1
  in Matrix.get v1 g.board
    |> M.unwrap
      (Result.Err (BishopMoveMoveError (OutOfBounds v1)))
      (\mp -> case mp of
        Nothing ->
          if 0 < j
          then bishopMoveForward v1 d j g
          else Result.Ok v1
        Just p  ->
          if 0 < j
          then Result.Err (BishopBlocked d v1)
          else if piecePlayer p == pl
          then Result.Err (BishopMoveMoveError (PathBlocked v1))
          else Result.Ok v1
      )

bishopMove : V2 -> DiagonalDirection -> Int -> Game -> Result BishopMoveError Board
bishopMove v0 d i g =
  let pl = gameTurn g
  in bishopMoveForward v0 d i g
    |> Result.andThen
      (\vf ->
        findKing pl g.board
        |> M.unwrap
          (Result.Err (BishopMoveMoveError PlayerHasNoKing))
          (\vk ->
            let nb  = reposition v0 vf g.board
                kcs = inCheck pl nb vk
            in if List.isEmpty kcs
            then Result.Ok nb
            else Result.Err (BishopMoveLeavesKingInCheck (KingInCheck kcs))
          )
      )

type RookMove
  = RookMove V2 StraightDirection Int

type RookMoveError
  = RookMoveLeavesKingInCheck KingInCheck
  | RookBlocked StraightDirection V2
  | RookMoveMoveError RegularMoveError


rookMoveForward : V2 -> StraightDirection -> Int -> Game -> Result RookMoveError V2
rookMoveForward v0 d i g =
  let pl = gameTurn g
      v1 = translateStraight d v0
      j  = i - 1
  in Matrix.get v1 g.board
    |> M.unwrap
      (Result.Err (RookMoveMoveError (OutOfBounds v1)))
      (\mp -> case mp of
        Nothing ->
          if 0 < j
          then rookMoveForward v1 d j g
          else Result.Ok v1
        Just p  ->
          if 0 < j
          then Result.Err (RookBlocked d v1)
          else if piecePlayer p == pl
          then Result.Err (RookMoveMoveError (PathBlocked v1))
          else Result.Ok v1
      )

-- TODO Accumulate Lazy Move Results ?
rookMove : V2 -> StraightDirection -> Int -> Game -> Result RookMoveError Board
rookMove v0 d i g =
  let pl = gameTurn g
  in rookMoveForward v0 d i g
    |> Result.andThen
      (\vf ->
        findKing pl g.board
        |> M.unwrap
          (Result.Err (RookMoveMoveError PlayerHasNoKing))
          (\vk ->
            let nb  = reposition v0 vf g.board
                kcs = inCheck pl nb vf
            in if List.isEmpty kcs
            then Result.Ok nb
            else Result.Err (RookMoveLeavesKingInCheck (KingInCheck kcs))
          )
      )
  

type QueenMove = QueenMove V2 Direction Int

type QueenMoveError
  = QueenMoveLeavesKingInCheck KingInCheck
  | QueenBlocked Direction V2
  | QueenMoveMoveError RegularMoveError


queenMoveForward : V2 -> Direction -> Int -> Game -> Result QueenMoveError V2
queenMoveForward v0 d i g =
  let pl = gameTurn g
      v1 = translate d v0
      j  = i - 1
  in Matrix.get v1 g.board
    |> M.unwrap
      (Result.Err (QueenMoveMoveError (OutOfBounds v1)))
      (\mp -> case mp of
        Nothing ->
          if 0 < j
          then queenMoveForward v1 d j g
          else Result.Ok v1
        Just p  ->
          if 0 < j
          then Result.Err (QueenBlocked d v1)
          else if piecePlayer p == pl
          then Result.Err (QueenMoveMoveError (PathBlocked v1))
          else Result.Ok v1
      )

-- TODO Accumulate Lazy Move Results ?
queenMove : V2 -> Direction -> Int -> Game -> Result QueenMoveError Board
queenMove v0 d i g =
  let pl = gameTurn g
  in queenMoveForward v0 d i g
    |> Result.andThen
      (\vf ->
        findKing pl g.board
        |> M.unwrap
          (Result.Err (QueenMoveMoveError PlayerHasNoKing))
          (\vk ->
            let nb  = reposition v0 vf g.board
                kcs = inCheck pl nb vf
            in if List.isEmpty kcs
            then Result.Ok nb
            else Result.Err (QueenMoveLeavesKingInCheck (KingInCheck kcs))
          )
      )


type KingMove
  = KingMove V2 Direction
  | KingCastling Castling

type KingMoveError
  = KingMoveLeavesKingInCheck KingInCheck
  | KingMoveMoveError RegularMoveError

kingMove : V2 -> Direction -> Game -> Result KingMoveError Board
kingMove v0 d g =
  let pl = gameTurn g
      vf = translate d v0
  in
    Matrix.get vf g.board
    |> M.unwrap
      (Result.Err (KingMoveMoveError (OutOfBounds vf)))
      (\mp -> case mp of
        Nothing ->
          let nb  = reposition v0 vf g.board
              kcs = inCheck pl nb vf
          in if List.isEmpty kcs
          then Result.Ok nb
          else Result.Err (KingMoveLeavesKingInCheck (KingInCheck kcs))
        Just p  ->
          if piecePlayer p == pl
          then Result.Err (KingMoveMoveError (PathBlocked vf))
          else
            let nb  = reposition v0 vf g.board
                kcs = inCheck pl nb vf
            in if List.isEmpty kcs
            then Result.Ok nb
            else Result.Err (KingMoveLeavesKingInCheck (KingInCheck kcs))
      )

type KingCastlingError
  = KingCastlingMoveError RegularMoveError
  | KingCastlingUnavailable

kingCastling : Castling -> Game -> Result KingCastlingError Board
kingCastling c g =
  let b  = g.board
      pl = gameTurn g
      r  = castlingRank pl
      { available } = case pl of
        White -> { available = g.whiteCastlingAvailable }
        Black -> { available = g.blackCastlingAvailable }
  in case c of
    KingSide ->
      let clearFiles = List.range 5 6
          checkFiles = List.range 4 6
      in if available.kingSide
      && (clearFiles
        |> List.filterMap (\f -> Matrix.get (f, r) b |> M.join)
        |> List.isEmpty
        )
      && (checkFiles
        |> List.map (\f -> inCheck pl b (f, r))
        |> List.concat
        |> List.isEmpty
        )
      then
        b
        |> tryReposition (4, r) (6, r)
        |> Maybe.andThen (tryReposition (7, r) (5, r))
        -- REVIEW
        |> Result.fromMaybe KingCastlingUnavailable
      else Result.Err KingCastlingUnavailable
    QueenSide ->
      let clearFiles = List.range 1 3
          checkFiles = List.range 2 4
      in if available.queenSide
      && (clearFiles
        |> List.filterMap (\f -> Matrix.get (f, r) b |> M.join)
        |> List.isEmpty
        )
      && (checkFiles
        |> List.map (\f -> inCheck pl b (f, r))
        |> List.concat
        |> List.isEmpty
        )
      then
        b
        |> tryReposition (4, r) (2, r)
        |> Maybe.andThen (tryReposition (0, r) (3, r))
        -- REVIEW
        |> Result.fromMaybe KingCastlingUnavailable
      else Result.Err KingCastlingUnavailable


kingLegalMoves : V2 -> Game -> List KingMove
kingLegalMoves v0 g =
  directions
  |> List.filterMap
    (\d ->
      kingMove v0 d g
      |> Result.toMaybe
      |> Maybe.map (always (KingMove v0 d))
    )
  
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

enPassantRank : Player -> Int
enPassantRank = player 5 2

pawnsRank : Player -> Int
pawnsRank = player 1 6

type alias Board = Matrix.Matrix (Maybe Piece)

initBoard : Board
initBoard = Matrix.repeat (8, 8) Nothing

type alias CastlingAvailable =
  { kingSide  : Bool
  , queenSide : Bool
  }

type alias HasQueenSide a = { a | queenSide : Bool }
type alias HasKingSide  a = { a | kingSide : Bool }

asQueenSideIn : HasQueenSide a -> Bool -> HasQueenSide a
asQueenSideIn a s = { a | queenSide = s }

asKingSideIn : HasKingSide a -> Bool -> HasKingSide a
asKingSideIn a s = { a | kingSide = s }

queenSideRookFile : Int
queenSideRookFile = 0

kingSideRookFile : Int
kingSideRookFile = 7

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
  , moves : List PieceMove
  , whiteCastlingAvailable : CastlingAvailable
  , blackCastlingAvailable : CastlingAvailable
  }

type alias HasWhiteCastlingAvailable a = { a | whiteCastlingAvailable : CastlingAvailable }

asWhiteCastlingAvailableIn : HasWhiteCastlingAvailable a -> CastlingAvailable -> HasWhiteCastlingAvailable a
asWhiteCastlingAvailableIn a s = { a | whiteCastlingAvailable = s }

type alias HasBlackCastlingAvailable a = { a | blackCastlingAvailable : CastlingAvailable }

asBlackCastlingAvailableIn : HasBlackCastlingAvailable a -> CastlingAvailable -> HasBlackCastlingAvailable a
asBlackCastlingAvailableIn a s = { a | blackCastlingAvailable = s }

gameTurn : { g | moves : List PieceMove } -> Player
gameTurn g = if remainderBy 2 (List.length g.moves) == 0 then White else Black

asBoardIn : { a | board : Board } -> Board -> { a | board : Board }
asBoardIn a x = { a | board = x }

pushMove : PieceMove -> { a | moves : List PieceMove } -> { a | moves : List PieceMove }
pushMove m a = { a | moves = m :: a.moves }

type alias Rank = Int
type alias File = Int
type Tile = Tile V2 Piece

advanceTurn : { a | turn : Player } -> { a | turn : Player }
advanceTurn a = { a | turn = opponent a.turn }

findKing : Player -> Board -> Maybe V2
findKing pl =
  Matrix.toIndexedList
  >> L.find (Tuple.second >> M.filter ((==) (Piece pl King)) >> M.isJust)
  >> Maybe.map Tuple.first

-- isStraightAttacker
isStraightAttacker : PieceType -> Bool
isStraightAttacker p = case p of
  Queen -> True
  Rook  -> True
  _     -> False

inStraightCheck : Player -> Board -> V2 -> StraightDirection -> Maybe Tile
inStraightCheck pl b v0 d =
  let vf = translateStraight d v0
  in Matrix.get vf b
    |> Maybe.andThen
      (\mp -> case mp of
        Nothing -> inStraightCheck pl b vf d
        Just p  ->
          if pl /= piecePlayer p && isStraightAttacker (pieceType p)
          then Just (Tile vf p)
          else Nothing
      )

inStraightsCheck : Player -> Board -> V2 -> List Tile
inStraightsCheck pl b v = List.filterMap (inStraightCheck pl b v) straightDirections


isDiagonalAttacker : PieceType -> Bool
isDiagonalAttacker p = case p of
  Queen  -> True
  Bishop -> True
  _      -> False

inDiagonalCheck : Player -> Board -> V2 -> DiagonalDirection -> Maybe Tile
inDiagonalCheck pl b v0 d =
  let vf = translateDiagonal d v0
  in Matrix.get vf b
    |> Maybe.andThen
      (\mp -> case mp of
        Nothing -> inDiagonalCheck pl b vf d
        Just p ->
          if pl /= piecePlayer p && isDiagonalAttacker (pieceType p)
          then Just (Tile vf p)
          else Nothing
      )

inDiagonalsCheck : Player -> Board -> V2 -> List Tile
inDiagonalsCheck pl b v = List.filterMap (inDiagonalCheck pl b v) diagonalDirections


inKingStraightOneCheck : Player -> Board -> V2 -> StraightDirection -> Maybe Tile
inKingStraightOneCheck pl b v0 d =
  let vf = translateStraight d v0
  in Matrix.get vf b
    |> M.join
    |> M.filter (\p -> pl /= piecePlayer p && King == pieceType p)
    |> Maybe.map (Tile vf)

inKingDiagonalOneCheck : Player -> Board -> V2 -> DiagonalDirection -> Maybe Tile
inKingDiagonalOneCheck pl b v0 d =
  let vf = translateDiagonal d v0
  in Matrix.get vf b
    |> M.join
    |> M.filter (\p -> pl /= piecePlayer p && King == pieceType p)
    |> Maybe.map (Tile vf)

inKingCheck : Player -> Board -> V2 -> List Tile
inKingCheck pl b v =
  let straightOneCheck = inKingStraightOneCheck pl b v
      diagonalOneCheck = inKingDiagonalOneCheck pl b v
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
inKnightOneCheck pl b v0 sD dD =
  let vf = translateStraight sD <| translateDiagonal dD v0
  in Matrix.get vf b
    |> M.join
    |> M.filter (\p -> pl /= piecePlayer p && Knight == (pieceType p))
    |> Maybe.map (Tile vf)

inKnightCheck : Player -> Board -> V2 -> List Tile
inKnightCheck pl b v = let check = inKnightOneCheck pl b v in
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
inPawnOneCheck pl b v0 d =
  let vf = translateDiagonal d v0
  in Matrix.get vf b
    |> M.join
    |> M.filter (\p -> pl /= piecePlayer p && Pawn == pieceType p)
    |> Maybe.map (Tile vf)

inPawnCheck : Player -> Board -> V2 -> List Tile
inPawnCheck pl b v = pl
  |> player [ NE, NW ] [ SE, SW ]
  |> List.map (inPawnOneCheck pl b v)
  |> List.filterMap identity

inCheck : Player -> Board -> V2 -> List Tile
inCheck pl b v =
  Matrix.get v b
  |> M.join
  |> M.filter (piecePlayer >> (/=) pl)
  |> M.unwrap
    [ inPawnCheck
    , inKnightCheck
    , inDiagonalsCheck
    , inStraightsCheck
    , inKingCheck
    ]
    (always [])
  |> List.map (\f -> f pl b v)
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
  = PawnAdvancePieceMoveError PawnAdvanceError
  | PawnCapturePieceMoveError PawnCaptureError
  | PawnPromotionPieceMoveError PawnPromotionError
  | PawnEnPassantPieceMoveError PawnEnPassantError
  | QueenMovePieceMoveError QueenMoveError
  | RookMovePieceMoveError RookMoveError
  | BishopMovePieceMoveError BishopMoveError
  | KnightMovePieceMoveError KnightMoveError
  | KingMovePieceMoveError KingMoveError
  | KingCastlingPieceMoveError KingCastlingError
  | OutOfBoundsMoveError V2
  | MissingPieceMoveError
  | IncorrectPieceTypeMoveError PieceType
  | IncorrectPlayerMoveError Player

-- type IlegalMove = LeavesPlayerInCheck

-- TODO Result IlegalMove (Translate (x, y) (x, y))
-- legalMove
tryMove : PieceMove -> Game -> Result MoveError Game
tryMove m g =
  (case m of
    PawnPieceMove pp -> case pp of
      PawnAdvance v          ->
        validateMovePiece v Pawn g
        |> Result.andThen (pawnAdvance v 1 >> Result.mapError PawnAdvancePieceMoveError)
        |> Result.map (asBoardIn g)
      PawnDoubleAdvance f    ->
        let v = (f, pawnsRank (gameTurn g))
        in validateMovePiece v Pawn g
        |> Result.andThen (pawnAdvance v 2 >> Result.mapError PawnAdvancePieceMoveError)
        |> Result.map (asBoardIn g)
      PawnCapture v d        ->
        validateMovePiece v Pawn g
        |> Result.andThen (pawnCapture v d >> Result.mapError PawnCapturePieceMoveError)
        |> Result.map (asBoardIn g)
      PawnPromotion f0 ff pr ->
        let v = (f0, promotionRank (gameTurn g))
        in validateMovePiece v Pawn g
        |> Result.andThen (pawnPromotion f0 ff pr >> Result.mapError PawnPromotionPieceMoveError)
        |> Result.map (asBoardIn g)
      PawnEnPassant h        ->
        pawnEnPassant h g
        |> Result.mapError PawnEnPassantPieceMoveError
        |> Result.map (asBoardIn g)
    KnightPieceMove (KnightMove v sd dd) ->
      validateMovePiece v Knight g
      |> Result.andThen (knightMove v sd dd >> Result.mapError KnightMovePieceMoveError)
      |> Result.map (asBoardIn g)
    BishopPieceMove   (BishopMove v d i) ->
      validateMovePiece v Bishop g
      |> Result.andThen (bishopMove v d i >> Result.mapError BishopMovePieceMoveError)
      |> Result.map (asBoardIn g)
    RookPieceMove       (RookMove v d i) ->
      validateMovePiece v Rook g
      |> Result.andThen (rookMove v d i >> Result.mapError RookMovePieceMoveError)
      |> Result.map
        (asBoardIn g
        >>
          (\ng ->
            let f = Tuple.first v
            in if queenSideRookFile == f
            then False |>
              (case gameTurn g of
                White -> asKingSideIn ng.whiteCastlingAvailable  >> asWhiteCastlingAvailableIn ng
                Black -> asQueenSideIn ng.blackCastlingAvailable >> asBlackCastlingAvailableIn ng
              )
            else if kingSideRookFile == f
            then False |>
              (case gameTurn g of
                White -> asKingSideIn ng.whiteCastlingAvailable >> asWhiteCastlingAvailableIn ng
                Black -> asKingSideIn ng.blackCastlingAvailable >> asBlackCastlingAvailableIn ng
              )
            else ng
          )
        )
    QueenPieceMove     (QueenMove v d i) ->
      validateMovePiece v Queen g
      |> Result.andThen (queenMove v d i >> Result.mapError QueenMovePieceMoveError)
      |> Result.map (asBoardIn g)
    KingPieceMove pp ->
      (case pp of
        KingMove v d   ->
          validateMovePiece v King g
          |> Result.andThen (kingMove v d >> Result.mapError KingMovePieceMoveError)
          |> Result.map (asBoardIn g)
        -- TODO
        KingCastling c ->
          Result.mapError KingCastlingPieceMoveError (kingCastling c g)
          |> Result.map
            (asBoardIn g
            >>
              (\ng -> case gameTurn g of
                White -> { ng | whiteCastlingAvailable = castlingDisabled }
                Black -> { ng | blackCastlingAvailable = castlingDisabled }
              )
            )

      )
      -- |> Result.map (\ng -> { ng | moves = m :: ng.moves })
    Temp_TeleportMove v0 vf -> Result.Ok g
  )
  |> Result.map (\ng -> { ng | moves = m :: ng.moves })

validateMovePiece : V2 -> PieceType -> Game -> Result MoveError Game
validateMovePiece v t g =
  Matrix.get v g.board
  |> M.unwrap
    (Result.Err (OutOfBoundsMoveError v))
    (M.unwrap
      (Result.Err MissingPieceMoveError)
      (\p ->
        let pl = piecePlayer p
        in if pl /= gameTurn g
        then Result.Err (IncorrectPlayerMoveError pl)
        else case t of
          Pawn -> Result.Ok g
          _    -> Result.Err (IncorrectPieceTypeMoveError t)
      )
    )

-- accumulate : Traversable t => Applicative f => ...
play : List PieceMove -> Game -> Result MoveError Game
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
  let empty = Matrix.repeat (Matrix.size b) False
  in Matrix.get v b
  |> M.join
  |> M.unwrap
    empty
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
      _ -> Matrix.repeat (Matrix.size b) False
    )
