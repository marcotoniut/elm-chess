module Chess exposing (..)

import Direction exposing (..)
import Html.Attributes exposing (kind)
import Matrix
import Maybe.Extra as M
import Result.Extra as R
import List
import List.Extra as L
import Tuple
import Tuple2
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
      rf = Tuple.second v1
  in if rf == castlingRank (opponent pl)
  then Result.Err (PawnAdvanceMoveError (OutOfBounds v1))
  else Matrix.get v1 g.board
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
pawnAdvance : V2 -> Int -> Game -> Result PawnAdvanceError (V2, Board)
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
          then Result.Ok (vf, nb)
          else Result.Err (PawnAdvanceLeavesKingInCheck (KingInCheck kcs))
        )
    )

pawnLegalAdvances : V2 -> Game -> List (V2, PawnMove)
pawnLegalAdvances v0 g =
  pawnAdvance v0 1 g
  |> R.unwrap [] (\(v, _) -> List.singleton (v, PawnAdvance v0))
  

pawnLegalDoubleAdvances : V2 -> Game -> List (V2, PawnMove)
pawnLegalDoubleAdvances (f, r) g =
  let pl = gameTurn g
  in if r /= pawnsRank pl
  then []
  else pawnAdvance (f, r) 2 g
    |> Result.toMaybe
    |> Maybe.map (Tuple.first)
    |> M.unwrap [] (\v -> List.singleton (v, PawnDoubleAdvance f))


type PawnCaptureError
  = PawnCaptureNoTarget HorizontalDirection
  | PawnCaptureLeavesKingInCheck KingInCheck
  | PawnCaptureMoveError RegularMoveError

pawnCapture : V2 -> HorizontalDirection -> Game -> Result PawnCaptureError (V2, Board)
pawnCapture v0 h g =
  let pl = gameTurn g
      d  = pl |> case h of
            Left  -> player NW SW
            Right -> player NE SE
      vf = translateDiagonal d v0
      rf = Tuple.second vf
  in if rf == castlingRank (opponent pl)
  then (Result.Err (PawnCaptureMoveError (OutOfBounds vf)))
  else Matrix.get vf g.board
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
              then Result.Ok (vf, nb)
              else Result.Err (PawnCaptureLeavesKingInCheck (KingInCheck kcs))
            )
      )

pawnLegalCaptures : V2 -> Game -> List (V2, PawnMove)
pawnLegalCaptures v0 g =
  [ Left, Right ]
  |> List.filterMap
    (\d ->
      pawnCapture v0 d g
      |> Result.toMaybe
      |> Maybe.map (\(v, _) -> (v, PawnCapture v0 d))
    )


type PawnEnPassantError
  = PawnEnPassantUnavailable
  | PawnEnPassantNoAttacker V2
  | PawnEnPassantLeavesKingInCheck KingInCheck
  | PawnEnPassantMoveError RegularMoveError

pawnEnPassant : HorizontalDirection -> Game -> Result PawnEnPassantError (V2, Board)
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
                      let vc = translateStraight (horizontalToStraight h) v0
                          vf = translateStraight (player N S pl) vc
                          nb = reposition v0 vf b |> Matrix.set vc Nothing
                          cs = inCheck pl nb vk
                      in if List.isEmpty cs
                      then Result.Ok (vf, nb)
                      else Result.Err (PawnEnPassantLeavesKingInCheck (KingInCheck cs))
                    )
                _ -> Result.Err (PawnEnPassantNoAttacker v0)
            )
      _ -> Result.Err PawnEnPassantUnavailable

pawnLegalEnPassants : Int -> Game -> List (V2, PawnMove)
pawnLegalEnPassants f g =
  [ Left, Right ]
  |> List.filterMap
    (\d ->
      pawnEnPassant d g
      |> Result.toMaybe
      |> Maybe.map Tuple.first
      |> M.filter (Tuple.first >> (==) (translateHorizontal d f))
      |> Maybe.map (\v -> (v, PawnEnPassant d))
    )

type PawnPromotionError
  = PawnPromotionBlocked Int Piece
  | PawnPromotionCaptureNoTarget HorizontalDirection
  | PawnPromotionLeavesKingInCheck KingInCheck
  | PawnPromotionMoveError RegularMoveError

pawnPromotion : Int -> Int -> PawnPromotion -> Game -> Result PawnPromotionError (V2, Board)
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
            |> Tuple.pair vf
            |> Result.Ok
          -- TODO
          -- else Result.Err InvalidPawnCapture
          else Result.Ok (vf, b)
        Just pf ->
          if f0 /= ff
          then
            if piecePlayer pf /= pl
            then
              Matrix.set v0 Nothing b
              |> Matrix.set vf (Just (Piece pl (promote pr)))
              |> Tuple.pair vf
              |> Result.Ok
            else Result.Err (PawnPromotionMoveError (PathBlocked vf))
          -- TODO
          -- else Result.Err (PawnPromotionMoveError InvalidPawnForward)
          else Result.Ok (vf, b)

pawnLegalMoves : V2 -> Game -> List (V2, PawnMove)
pawnLegalMoves v g =
  let f = Tuple.first v in
  [ pawnLegalAdvances v g
  , pawnLegalDoubleAdvances v g
  , pawnLegalCaptures v g
  , pawnLegalEnPassants f g
  ]
  |> List.concat


type KnightMove
  = KnightMove V2 StraightDirection HorizontalDirection

type KnightMoveError
  = KnightMoveLeavesKingInCheck KingInCheck
  | KnightMoveMoveError RegularMoveError

knightMoveTarget : V2 -> StraightDirection -> HorizontalDirection -> V2
knightMoveTarget v0 sd hd = translateDiagonal (turnDiagonal sd hd) <| translateStraight sd v0

knightMove : V2 -> StraightDirection -> HorizontalDirection -> Game -> Result KnightMoveError (V2, Board)
knightMove v0 sd hd g =
  let vf = knightMoveTarget v0 sd hd
      pl = gameTurn g
  in
    findKing pl g.board
    |> M.unwrap (Result.Err (KnightMoveMoveError (PlayerHasNoKing)))
      (\vk -> Matrix.get vf g.board
      |> M.unwrap
        (Result.Err (KnightMoveMoveError (OutOfBounds vf)))
        (\mp -> case mp of
          Nothing ->
            let nb  = reposition v0 vf g.board
                kcs = inCheck pl nb vk
            in if List.isEmpty kcs
            then Result.Ok (vf, nb)
            else Result.Err (KnightMoveLeavesKingInCheck (KingInCheck kcs))
          Just p  ->
            if piecePlayer p == pl
            then Result.Err (KnightMoveMoveError (PathBlocked vf))
            else
              let nb  = reposition v0 vf g.board
                  kcs = inCheck pl nb vk
              in if List.isEmpty kcs
              then Result.Ok (vf, nb)
              else Result.Err (KnightMoveLeavesKingInCheck (KingInCheck kcs))
        )
      )

knightLegalMoves : V2 -> Game -> List (V2, KnightMove)
knightLegalMoves v g =
  -- TODO? KnightMove N Left v
  [ KnightMove v N Left
  , KnightMove v N Right
  , KnightMove v E Left
  , KnightMove v E Right
  , KnightMove v S Left
  , KnightMove v S Right
  , KnightMove v W Left
  , KnightMove v W Right
  ]
  |> List.filterMap
    (\m -> m
    |> (\(KnightMove v0 sd hd) -> knightMove v0 sd hd g)
    |> Result.toMaybe
    |> Maybe.map (\(vf, _) -> (vf, m))
    -- |> Maybe.map (always m)
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

-- bishopMoveTarget : V2 -> DiagonalDirection -> Int -> V2
-- bishopMoveTarget v d n = List.foldl (\_ -> translateDiagonal d) v (List.range 0 (n - 1))

bishopMove : V2 -> DiagonalDirection -> Int -> Game -> Result BishopMoveError (V2, Board)
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
            then Result.Ok (vf, nb)
            else Result.Err (BishopMoveLeavesKingInCheck (KingInCheck kcs))
          )
      )

bishopDirectionLegalMoves : Int -> V2 -> DiagonalDirection -> Game -> List (V2, BishopMove)
bishopDirectionLegalMoves i v0 d g =
  let pl = gameTurn g
      v1 = List.foldl (\_ -> translateDiagonal d) v0 (List.range 0 i)
      j = i + 1
  in findKing pl g.board
    |> M.unwrap
      []
      (\vk ->
        Matrix.get v1 g.board
        |> M.unwrap
          []
          (\mp -> case mp of
            Nothing ->
              let nb  = reposition v0 v1 g.board
                  kcs = inCheck pl nb vk
                  rs  = bishopDirectionLegalMoves j v0 d g
              in if List.isEmpty kcs
              then (v1, BishopMove v0 d j) :: rs
              else rs
            Just p  ->
              if piecePlayer p == pl
              then []
              else
                let nb  = reposition v0 v1 g.board
                    kcs = inCheck pl nb vk
                in if List.isEmpty kcs
                then [ (v1, BishopMove v0 d j) ]
                else []
          )
      )

bishopLegalMoves : V2 -> Game -> List (V2, BishopMove)
bishopLegalMoves v0 g =
  diagonalDirections
  |> List.map (\d -> bishopDirectionLegalMoves 0 v0 d g)
  |> List.concat


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

-- rookMoveTarget : V2 -> StraightDirection -> Int -> V2
-- rookMoveTarget v0 d n = List.foldl (\_ -> translateStraight d) v0 (List.range 0 (n - 1))

-- TODO Accumulate Lazy Move Results ?
rookMove : V2 -> StraightDirection -> Int -> Game -> Result RookMoveError (V2, Board)
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
                kcs = inCheck pl nb vk
            in if List.isEmpty kcs
            then Result.Ok (vf, nb)
            else Result.Err (RookMoveLeavesKingInCheck (KingInCheck kcs))
          )
      )

rookDirectionLegalMoves : Int -> V2 -> StraightDirection -> Game -> List (V2, RookMove)
rookDirectionLegalMoves i v0 d g =
  let pl = gameTurn g
      v1 = List.foldl (\_ -> translateStraight d) v0 (List.range 0 i)
      j = i + 1
  in findKing pl g.board
    |> M.unwrap
      []
      (\vk ->
        Matrix.get v1 g.board
        |> M.unwrap
          []
          (\mp -> case mp of
            Nothing ->
              let nb  = reposition v0 v1 g.board
                  kcs = inCheck pl nb vk
                  rs  = rookDirectionLegalMoves j v0 d g
              in if List.isEmpty kcs
              then (v1, RookMove v0 d j) :: rs
              else rs
            Just p  ->
              if piecePlayer p == pl
              then []
              else
                let nb  = reposition v0 v1 g.board
                    kcs = inCheck pl nb vk
                in if List.isEmpty kcs
                then [ (v1, RookMove v0 d j) ]
                else []
          )
      )

rookLegalMoves : V2 -> Game -> List (V2, RookMove)
rookLegalMoves v0 g =
  straightDirections
  |> List.map (\d -> rookDirectionLegalMoves 0 v0 d g)
  |> List.concat


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

-- queenMoveTarget : V2 -> Direction -> Int -> V2
-- queenMoveTarget v d n = List.foldl (\_ -> translate d) v (List.range 0 (n - 1))

-- TODO Accumulate Lazy Move Results ?
queenMove : V2 -> Direction -> Int -> Game -> Result QueenMoveError (V2, Board)
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
                kcs = inCheck pl nb vk
            in if List.isEmpty kcs
            then Result.Ok (vf, nb)
            else Result.Err (QueenMoveLeavesKingInCheck (KingInCheck kcs))
          )
      )

queenDirectionLegalMoves : Int -> V2 -> Direction -> Game -> List (V2, QueenMove)
queenDirectionLegalMoves i v0 d g =
  let pl = gameTurn g
      v1 = List.foldl (\_ -> translate d) v0 (List.range 0 i)
      j = i + 1
  in findKing pl g.board
    |> M.unwrap
      []
      (\vk ->
        Matrix.get v1 g.board
        |> M.unwrap
          []
          (\mp -> case mp of
            Nothing ->
              let nb  = reposition v0 v1 g.board
                  kcs = inCheck pl nb vk
                  rs  = queenDirectionLegalMoves j v0 d g
              in if List.isEmpty kcs
              then (v1, QueenMove v0 d j) :: rs
              else rs
            Just p  ->
              if piecePlayer p == pl
              then []
              else
                let nb  = reposition v0 v1 g.board
                    kcs = inCheck pl nb vk
                in if List.isEmpty kcs
                then [ (v1, QueenMove v0 d j) ]
                else []
          )
      )


queenLegalMoves : V2 -> Game -> List (V2, QueenMove)
queenLegalMoves v0 g =
  directions
  |> List.map (\d -> queenDirectionLegalMoves 0 v0 d g)
  |> List.concat


type KingMove
  = KingMove V2 Direction
  | KingCastling Castling

type KingMoveError
  = KingMoveLeavesKingInCheck KingInCheck
  | KingMoveMoveError RegularMoveError

-- kingMoveTarget : V2 -> Direction -> V2
-- kingMoveTarget v d = translate d v

kingMove : V2 -> Direction -> Game -> Result KingMoveError (V2, Board)
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
          then Result.Ok (vf, nb)
          else Result.Err (KingMoveLeavesKingInCheck (KingInCheck kcs))
        Just p  ->
          if piecePlayer p == pl
          then Result.Err (KingMoveMoveError (PathBlocked vf))
          else
            let nb  = reposition v0 vf g.board
                kcs = inCheck pl nb vf
            in if List.isEmpty kcs
            then Result.Ok (vf, nb)
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

kingCastlingTarget : Castling -> Player -> V2
kingCastlingTarget c pl = Tuple2.pairTo (castlingRank pl) <| case c of
  KingSide  -> 6
  QueenSide -> 2
  

kingLegalMoves : V2 -> Game -> List (V2, KingMove)
kingLegalMoves v0 g =
  let pl = gameTurn g
  in directions
  |> List.map
    (\d ->
      kingMove v0 d g
      |> Result.toMaybe
      |> Maybe.map (\(vf, _) -> (vf, KingMove v0 d))
    )
  |> (::)
      (kingCastling QueenSide g
      |> Result.toMaybe
      |> Maybe.map (always (kingCastlingTarget QueenSide pl, KingCastling QueenSide))
      )
  |> (::)
      (kingCastling KingSide g
      |> Result.toMaybe
      |> Maybe.map (always (kingCastlingTarget KingSide pl, KingCastling KingSide))
      )
  |> List.filterMap identity
  
type PieceMove
  = PawnPieceMove   PawnMove
  | KnightPieceMove KnightMove
  | BishopPieceMove BishopMove
  | RookPieceMove   RookMove
  | QueenPieceMove  QueenMove
  | KingPieceMove   KingMove

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
enPassantRank = player 4 3

pawnsRank : Player -> Int
pawnsRank = player 1 6

pawnDoubleAdvanceRank : Player -> Int
pawnDoubleAdvanceRank pl = 2 * (player 1 -1 pl) + pawnsRank pl

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
    |> M.filter (\p -> pl /= piecePlayer p && Knight == pieceType p)
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
        |> Result.map (Tuple.second >> asBoardIn g)
      PawnDoubleAdvance f    ->
        let v = (f, pawnsRank (gameTurn g))
        in validateMovePiece v Pawn g
        |> Result.andThen (pawnAdvance v 2 >> Result.mapError PawnAdvancePieceMoveError)
        |> Result.map (Tuple.second >> asBoardIn g)
      PawnCapture v d        ->
        validateMovePiece v Pawn g
        |> Result.andThen (pawnCapture v d >> Result.mapError PawnCapturePieceMoveError)
        |> Result.map (Tuple.second >> asBoardIn g)
      PawnPromotion f0 ff pr ->
        let v = (f0, promotionRank (gameTurn g))
        in validateMovePiece v Pawn g
        |> Result.andThen (pawnPromotion f0 ff pr >> Result.mapError PawnPromotionPieceMoveError)
        |> Result.map (Tuple.second >> asBoardIn g)
      PawnEnPassant h        ->
        pawnEnPassant h g
        |> Result.mapError PawnEnPassantPieceMoveError
        |> Result.map (Tuple.second >> asBoardIn g)
    KnightPieceMove (KnightMove v sd dd) ->
      validateMovePiece v Knight g
      |> Result.andThen (knightMove v sd dd >> Result.mapError KnightMovePieceMoveError)
      |> Result.map (Tuple.second >> asBoardIn g)
    BishopPieceMove   (BishopMove v d i) ->
      validateMovePiece v Bishop g
      |> Result.andThen (bishopMove v d i >> Result.mapError BishopMovePieceMoveError)
      |> Result.map (Tuple.second >> asBoardIn g)
    RookPieceMove       (RookMove v d i) ->
      validateMovePiece v Rook g
      |> Result.andThen (rookMove v d i >> Result.mapError RookMovePieceMoveError)
      |> Result.map
        (Tuple.second
        >> asBoardIn g
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
      |> Result.map (Tuple.second >> asBoardIn g)
    KingPieceMove pp ->
      (case pp of
        KingMove v d   ->
          validateMovePiece v King g
          |> Result.andThen (kingMove v d >> Result.mapError KingMovePieceMoveError)
          |> Result.map (Tuple.second >> asBoardIn g)
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
        else let pt = pieceType p
        in if t == pt
        then Result.Ok g
        else Result.Err (IncorrectPieceTypeMoveError pt)
      )
    )
    -- TODO Filter

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

-- TODO
pieceLegalMoves : Game -> V2 -> Piece -> List (V2, PieceMove)
pieceLegalMoves g v (Piece pl p) = case p of
  Pawn   -> List.map (Tuple.mapSecond PawnPieceMove)   <| pawnLegalMoves   v g
  Knight -> List.map (Tuple.mapSecond KnightPieceMove) <| knightLegalMoves v g
  Bishop -> List.map (Tuple.mapSecond BishopPieceMove) <| bishopLegalMoves v g
  Rook   -> List.map (Tuple.mapSecond RookPieceMove)   <| rookLegalMoves   v g
  Queen  -> List.map (Tuple.mapSecond QueenPieceMove)  <| queenLegalMoves  v g
  King   -> List.map (Tuple.mapSecond KingPieceMove)   <| kingLegalMoves   v g


playerPieces : Player -> Board -> List (V2, Piece)
playerPieces pl
  = Matrix.toIndexedList
  >> List.filterMap
    (\(v, mp) -> M.filter (piecePlayer >> (==) pl) mp
    |> Maybe.map (Tuple.pair v)
    )

-- type GameStatus
--   = Invalid
--   | Normal
--   | Check Player
--   | CheckMate Player
--   | StaleMate

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

-- checkMatrix : V2 -> Board -> Matrix.Matrix Bool
-- checkMatrix v b =
--   let empty = Matrix.repeat (Matrix.size b) False
--   in Matrix.get v b
--   |> M.join
--   |> M.unwrap
--     empty
--     (\(Piece pl t) -> case t of
--       Bishop -> diagonalDirections
--         |> List.map (checkDiagonal pl b v)
--         |> List.concat
--         |> List.foldl (\(v1, _) -> Matrix.set v1 True) empty
--       Rook -> straightDirections
--         |> List.map (checkStraight pl b v)
--         |> List.concat
--         |> List.foldl (\(v1, _) -> Matrix.set v1 True) empty
--       Queen ->
--         [ List.map (checkStraight pl b v) straightDirections
--         , List.map (checkDiagonal pl b v) diagonalDirections
--         ]
--         |> List.concat
--         |> List.concat
--         |> List.foldl (\(v1, _) -> Matrix.set v1 True) empty
--       -- TODO Rest
--       --  { ne = checkDiagonal pl NE b v
--       --  , se = checkDiagonal pl SE b v
--       --  , sw = checkDiagonal pl SW b v
--       --  , nw = checkDiagonal pl NW b v
--       --  }
--       --  Rook   -> inStraightsCheck pl b v
--       _ -> Matrix.repeat (Matrix.size b) False
--     )
