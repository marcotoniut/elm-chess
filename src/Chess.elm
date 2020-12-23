module Chess exposing (..)

import Alphabet exposing (intToAlphabet)
import Direction exposing (..)
import Html.Attributes exposing (kind)
import Matrix
import Maybe.Extra as M
import Result.Extra as R
import List
import List.Extra as L
import Tuple
import Tuple2

type Player = White | Black
player : a -> a -> Player -> a
player w b p = case p of
  White -> w
  Black -> b
opponent : Player -> Player
opponent = player Black White

playerDirection : Player -> StraightDirection
playerDirection = player N S

type PieceType = King | Queen | Rook | Bishop | Knight | Pawn
type PawnPromotion = QueenPromotion | RookPromotion | BishopPromotion | KnightPromotion
promote : PawnPromotion -> PieceType
promote p = case p of
  QueenPromotion  -> Queen
  RookPromotion   -> Rook
  BishopPromotion -> Bishop
  KnightPromotion -> Knight

promotionFromPieceType : PieceType -> Maybe PawnPromotion
promotionFromPieceType p = case p of
  Queen   -> Just QueenPromotion
  Rook    -> Just RookPromotion
  Bishop  -> Just BishopPromotion
  Knight  -> Just KnightPromotion
  _       -> Nothing

type PawnPromotionMove
  = PawnPromotionAdvance Int
  | PawnPromotionCapture Int HorizontalDirection

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
  | PawnPromotion PawnPromotion PawnPromotionMove

type PawnAdvanceError
  = PawnAdvanceBlocked V2 Piece
  | PawnAdvanceLeavesKingInCheck KingInCheck
  | PawnAdvanceMoveError RegularMoveError

pawnAdvanceForward : V2 -> Int -> Game -> Result PawnAdvanceError V2
pawnAdvanceForward v0 i g =
  let pl = gameTurn g
      v1 = pawnAdvanceTarget pl v0
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
pawnAdvance v0 f g =
  let pl = gameTurn g
  in pawnAdvanceForward v0 f g
    |> Result.andThen
    (\vf ->
      findKing pl g.board
      |> M.unwrap
        (Result.Err (PawnAdvanceMoveError PlayerHasNoKing))
        (\vk ->
          let nb  = reposition v0 vf g.board
              kcs = inCheck (opponent pl) nb vk
          in if List.isEmpty kcs
          then Result.Ok (vf, nb)
          else Result.Err (PawnAdvanceLeavesKingInCheck (KingInCheck kcs))
        )
    )

pawnAdvanceTarget : Player -> V2 -> V2
pawnAdvanceTarget pl = translateStraight (playerDirection pl)

pawnDoubleAdvanceTarget : Player -> Int -> V2
pawnDoubleAdvanceTarget pl f =
  let v0 = (f, pawnsRank pl)
      d  = playerDirection pl
  in translateStraight d <| translateStraight d v0

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
      vf = pawnCaptureTarget pl v0 h
      rf = Tuple.second vf
  in if rf == castlingRank (opponent pl)
  then Result.Err (PawnCaptureMoveError (OutOfBounds vf))
  else Matrix.get vf g.board
    |> M.unwrap
      (Result.Err (PawnCaptureMoveError (OutOfBounds vf)))
      (\mp -> case mp of
        Nothing -> Result.Err <| PawnCaptureNoTarget h
        Just pf  ->
          if piecePlayer pf == pl
          then Result.Err (PawnCaptureMoveError (PathBlocked vf))
          else findKing pl g.board
            |> M.unwrap
              (Result.Err (PawnCaptureMoveError PlayerHasNoKing))
              (\vk ->
                let nb  = reposition v0 vf g.board
                    kcs = inCheck (opponent pl) nb vk
                in if List.isEmpty kcs
                then Result.Ok (vf, nb)
                else Result.Err (PawnCaptureLeavesKingInCheck (KingInCheck kcs))
            )
      )

pawnCaptureTarget : Player -> V2 -> HorizontalDirection -> V2
pawnCaptureTarget pl v0 h =
  let d = pl |> case h of
        Left  -> player NW SW
        Right -> player NE SE
  in translateDiagonal d v0

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
    Just  l -> case l.move of
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
                          vf = translateStraight (playerDirection pl) vc
                          nb = reposition v0 vf b |> Matrix.set vc Nothing
                          cs = inCheck (opponent pl) nb vk
                      in if List.isEmpty cs
                      then Result.Ok (vf, nb)
                      else Result.Err (PawnEnPassantLeavesKingInCheck (KingInCheck cs))
                    )
                _ -> Result.Err (PawnEnPassantNoAttacker v0)
            )
      _ -> Result.Err PawnEnPassantUnavailable

pawnEnPassantTarget : Player -> Int -> HorizontalDirection -> V2
pawnEnPassantTarget pl f h =
  let d = pl |> case h of
        Left  -> player NW SW
        Right -> player NE SE
  in translateDiagonal d (f, pawnsRank pl)

pawnLegalEnPassants : V2 -> Game -> List (V2, PawnMove)
pawnLegalEnPassants (f, r) g =
  if r /= enPassantRank (gameTurn g)
  then []
  else
    [ Left, Right ]
    |> List.filterMap
      (\d ->
        pawnEnPassant d g
        |> Result.toMaybe
        |> Maybe.map Tuple.first
        |> M.filter (Tuple.first >> (==) (translateHorizontal d f))
        |> Maybe.map (\v -> (v, PawnEnPassant d))
      )

pawnLegalMoves : V2 -> Game -> List (V2, PawnMove)
pawnLegalMoves v g =
  let f = Tuple.first v in
  [ pawnLegalAdvances v g
  , pawnLegalDoubleAdvances v g
  , pawnLegalCaptures v g
  , pawnLegalEnPassants v g
  ]
  |> List.concat


type PawnPromotionAdvanceError
  = PawnPromotionBlocked Int Piece

type PawnPromotionCaptureError
  = PawnPromotionCaptureNoTarget HorizontalDirection

type PawnPromotionError
  = PawnPromotionAdvanceError PawnPromotionAdvanceError
  | PawnPromotionCaptureError PawnPromotionCaptureError
  | PawnPromotionLeavesKingInCheck KingInCheck
  | PawnPromotionMoveError RegularMoveError

-- TODO kingInCheck
pawnPromotionAdvance : Int -> Game -> Result PawnPromotionError (Translation, Board)
pawnPromotionAdvance f g =
  let pl = gameTurn g
      b  = g.board
      pawnsR = pawnsRank <| opponent pl
      promotionR = promotionRank pl
      v0 = (f, pawnsR)
      vf = (f, promotionR)
  in case Matrix.get vf b of
    Nothing -> Result.Err (PawnPromotionMoveError (OutOfBounds vf))
    Just tf -> case tf of
      Nothing ->
        Matrix.set v0 Nothing b
        |> Matrix.set vf (Just (Piece pl Pawn))
        |> Tuple.pair (Translation v0 vf)
        |> Result.Ok
      Just pf -> Result.Err (PawnPromotionAdvanceError (PawnPromotionBlocked f pf))

pawnPromotionCapture : Int -> HorizontalDirection -> Game -> Result PawnPromotionError (Translation, Board)
pawnPromotionCapture f d g =
  let pl = gameTurn g
      b  = g.board
      pawnsR = pawnsRank <| opponent pl
      promotionR = promotionRank pl
      v0 = (f, pawnsR)
      vf = (translateHorizontal d f, promotionR)
  in case Matrix.get vf b of
    Nothing -> Result.Err (PawnPromotionMoveError (OutOfBounds vf))
    Just tf -> case tf of
      Nothing -> Result.Err (PawnPromotionCaptureError (PawnPromotionCaptureNoTarget d))
      Just pf ->
        if piecePlayer pf /= pl
        then
          Matrix.set v0 Nothing b
          |> Matrix.set vf (Just (Piece pl Pawn))
          |> Tuple.pair (Translation v0 vf)
          |> Result.Ok
        else Result.Err (PawnPromotionMoveError (PathBlocked vf))

pawnLegalPromotionAdvances : V2 -> Game -> List (V2, PawnPromotionMove)
pawnLegalPromotionAdvances (f, r) g =
  if r /= pawnsRank (opponent (gameTurn g))
  then []
  else pawnPromotionAdvance f g
    |> R.unwrap [] (\(Translation _ vf, _) -> List.singleton (vf, PawnPromotionAdvance f))
  

pawnLegalPromotionCaptures : V2 -> Game -> List (V2, PawnPromotionMove)
pawnLegalPromotionCaptures (f, r) g =
  if r /= pawnsRank (opponent (gameTurn g))
  then []
  else
    [ Left, Right ]
    |> List.filterMap
      (\d ->
        pawnPromotionCapture f d g
        |> Result.toMaybe
        |> Maybe.map (\(Translation _ vf, _) -> (vf, PawnPromotionCapture f d))
      )

pawnLegalPromotionMoves : V2 -> Game -> List (V2, PawnPromotionMove)
pawnLegalPromotionMoves v g =
  let f = Tuple.first v in
  [ pawnLegalPromotionAdvances v g
  , pawnLegalPromotionCaptures v g
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
                kcs = inCheck (opponent pl) nb vk
            in if List.isEmpty kcs
            then Result.Ok (vf, nb)
            else Result.Err (KnightMoveLeavesKingInCheck (KingInCheck kcs))
          Just p  ->
            if piecePlayer p == pl
            then Result.Err (KnightMoveMoveError (PathBlocked vf))
            else
              let nb  = reposition v0 vf g.board
                  kcs = inCheck (opponent pl) nb vk
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
                kcs = inCheck (opponent pl) nb vk
            in if List.isEmpty kcs
            then Result.Ok (vf, nb)
            else Result.Err (BishopMoveLeavesKingInCheck (KingInCheck kcs))
          )
      )

bishopMoveTarget : Int -> DiagonalDirection -> V2 -> V2
bishopMoveTarget n d v = if 0 < n then bishopMoveTarget (n - 1) d (translateDiagonal d v) else v

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
                  kcs = inCheck (opponent pl) nb vk
                  rs  = bishopDirectionLegalMoves j v0 d g
              in if List.isEmpty kcs
              then (v1, BishopMove v0 d j) :: rs
              else rs
            Just p  ->
              if piecePlayer p == pl
              then []
              else
                let nb  = reposition v0 v1 g.board
                    kcs = inCheck (opponent pl) nb vk
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
                kcs = inCheck (opponent pl) nb vk
            in if List.isEmpty kcs
            then Result.Ok (vf, nb)
            else Result.Err (RookMoveLeavesKingInCheck (KingInCheck kcs))
          )
      )

rookMoveTarget : Int -> StraightDirection -> V2 -> V2
rookMoveTarget n d v = if 0 < n then rookMoveTarget (n - 1) d (translateStraight d v) else v

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
                  kcs = inCheck (opponent pl) nb vk
                  rs  = rookDirectionLegalMoves j v0 d g
              in if List.isEmpty kcs
              then (v1, RookMove v0 d j) :: rs
              else rs
            Just p  ->
              if piecePlayer p == pl
              then []
              else
                let nb  = reposition v0 v1 g.board
                    kcs = inCheck (opponent pl) nb vk
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
                kcs = inCheck (opponent pl) nb vk
            in if List.isEmpty kcs
            then Result.Ok (vf, nb)
            else Result.Err (QueenMoveLeavesKingInCheck (KingInCheck kcs))
          )
      )

queenMoveTarget : Int -> Direction -> V2 -> V2
queenMoveTarget n d v = if 0 < n then queenMoveTarget (n - 1) d (translate d v) else v

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
                  kcs = inCheck (opponent pl) nb vk
                  rs  = queenDirectionLegalMoves j v0 d g
              in if List.isEmpty kcs
              then (v1, QueenMove v0 d j) :: rs
              else rs
            Just p  ->
              if piecePlayer p == pl
              then []
              else
                let nb  = reposition v0 v1 g.board
                    kcs = inCheck (opponent pl) nb vk
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
              kcs = inCheck (opponent pl) nb vf
          in if List.isEmpty kcs
          then Result.Ok (vf, nb)
          else Result.Err (KingMoveLeavesKingInCheck (KingInCheck kcs))
        Just p  ->
          if piecePlayer p == pl
          then Result.Err (KingMoveMoveError (PathBlocked vf))
          else
            let nb  = reposition v0 vf g.board
                kcs = inCheck (opponent pl) nb vf
            in if List.isEmpty kcs
            then Result.Ok (vf, nb)
            else Result.Err (KingMoveLeavesKingInCheck (KingInCheck kcs))
      )

kingMoveTarget : Direction -> V2 -> V2
kingMoveTarget = translate

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
        |> List.map (\f -> inCheck (opponent pl) b (f, r))
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
        |> List.map (\f -> inCheck (opponent pl) b (f, r))
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

type Castling = QueenSide | KingSide
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

type alias Move =
  { move : PieceMove
  , board : Board
  , whiteCastlingAvailable : CastlingAvailable
  , blackCastlingAvailable : CastlingAvailable
  }

type alias Game =
  { board : Board
  , moves : List Move
  , whiteCastlingAvailable : CastlingAvailable
  , blackCastlingAvailable : CastlingAvailable
  }

type alias HasWhiteCastlingAvailable a = { a | whiteCastlingAvailable : CastlingAvailable }

asWhiteCastlingAvailableIn : HasWhiteCastlingAvailable a -> CastlingAvailable -> HasWhiteCastlingAvailable a
asWhiteCastlingAvailableIn a s = { a | whiteCastlingAvailable = s }

type alias HasBlackCastlingAvailable a = { a | blackCastlingAvailable : CastlingAvailable }

asBlackCastlingAvailableIn : HasBlackCastlingAvailable a -> CastlingAvailable -> HasBlackCastlingAvailable a
asBlackCastlingAvailableIn a s = { a | blackCastlingAvailable = s }

gameTurn : { g | moves : List a } -> Player
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

isStraightAttacker : PieceType -> Bool
isStraightAttacker p = case p of
  Queen -> True
  Rook  -> True
  _     -> False

inStraightCheck : (PieceType -> Bool) -> Player -> Board -> V2 -> Int -> StraightDirection -> Maybe ((StraightDirection, Int), Tile)
inStraightCheck isOfPieceType pl b v0 i d =
  let vf = translateStraight d v0
      n  = i + 1
  in Matrix.get vf b
    |> Maybe.andThen
      (\mp -> case mp of
        Nothing -> inStraightCheck isOfPieceType pl b vf n d
        Just p  ->
          if piecePlayer p == pl && isOfPieceType (pieceType p)
          then Just ((d, n), Tile vf p)
          else Nothing
      )

inStraightsCheck : (PieceType -> Bool) -> Player -> Board -> V2 -> List ((StraightDirection, Int), Tile)
inStraightsCheck isOfPieceType pl b v = List.filterMap (inStraightCheck isOfPieceType pl b v 0) straightDirections

isDiagonalAttacker : PieceType -> Bool
isDiagonalAttacker p = case p of
  Queen  -> True
  Bishop -> True
  _      -> False

inDiagonalCheck : (PieceType -> Bool) -> Player -> Board -> V2 -> Int -> DiagonalDirection -> Maybe ((DiagonalDirection, Int), Tile)
inDiagonalCheck isOfPieceType pl b v0 i d =
  let vf = translateDiagonal d v0
      n = i + 1
  in Matrix.get vf b
    |> Maybe.andThen
      (\mp -> case mp of
        Nothing -> inDiagonalCheck isOfPieceType pl b vf n d
        Just p  ->
          if piecePlayer p == pl && isOfPieceType (pieceType p)
          then Just ((d, n), Tile vf p)
          else Nothing
      )

inDiagonalsCheck : (PieceType -> Bool) -> Player -> Board -> V2 -> List ((DiagonalDirection, Int), Tile)
inDiagonalsCheck isOfPieceType pl b v = List.filterMap (inDiagonalCheck isOfPieceType pl b v 0) diagonalDirections

inBishopCheck : Player -> Board -> V2 -> List ((DiagonalDirection, Int), Tile)
inBishopCheck pl b v =
  List.filterMap (inDiagonalCheck ((==) Bishop) pl b v 0) diagonalDirections

inRookCheck : Player -> Board -> V2 -> List ((StraightDirection, Int), Tile)
inRookCheck pl b v =
  List.filterMap (inStraightCheck ((==) Rook) pl b v 0) straightDirections

inQueenCheck : Player -> Board -> V2 -> List ((Direction, Int), Tile)
inQueenCheck pl b v =
  let dc = List.filterMap (inDiagonalCheck ((==) Queen) pl b v 0) diagonalDirections
      sc = List.filterMap (inStraightCheck ((==) Queen) pl b v 0) straightDirections
  in List.concat
    [ dc |> List.map (Tuple.mapFirst (Tuple.mapFirst DiagonalDirection))
    , sc |> List.map (Tuple.mapFirst (Tuple.mapFirst StraightDirection))
    ]


inKingStraightOneCheck : Player -> Board -> V2 -> StraightDirection -> Maybe (StraightDirection, Tile)
inKingStraightOneCheck pl b v0 d =
  let vf = translateStraight d v0
  in Matrix.get vf b
    |> M.join
    |> M.filter ((==) (Piece pl King))
    |> Maybe.map (Tile vf >> Tuple.pair d)

inKingDiagonalOneCheck : Player -> Board -> V2 -> DiagonalDirection -> Maybe (DiagonalDirection, Tile)
inKingDiagonalOneCheck pl b v0 d =
  let vf = translateDiagonal d v0
  in Matrix.get vf b
    |> M.join
    |> M.filter ((==) (Piece pl King))
    |> Maybe.map (Tile vf >> Tuple.pair d)

inKingCheck : Player -> Board -> V2 -> List (Direction, Tile)
inKingCheck pl b v =
  let straightOneCheck = inKingStraightOneCheck pl b v
      diagonalOneCheck = inKingDiagonalOneCheck pl b v
  in
  [ straightOneCheck N  |> Maybe.map (Tuple.mapFirst StraightDirection)
  , straightOneCheck E  |> Maybe.map (Tuple.mapFirst StraightDirection)
  , straightOneCheck S  |> Maybe.map (Tuple.mapFirst StraightDirection)
  , straightOneCheck W  |> Maybe.map (Tuple.mapFirst StraightDirection)
  , diagonalOneCheck NE |> Maybe.map (Tuple.mapFirst DiagonalDirection)
  , diagonalOneCheck SE |> Maybe.map (Tuple.mapFirst DiagonalDirection)
  , diagonalOneCheck SW |> Maybe.map (Tuple.mapFirst DiagonalDirection)
  , diagonalOneCheck NW |> Maybe.map (Tuple.mapFirst DiagonalDirection)
  ]
  |> List.filterMap identity


inKnightOneCheck : Player -> Board -> V2 -> StraightDirection -> HorizontalDirection -> Maybe ((StraightDirection, HorizontalDirection), Tile)
inKnightOneCheck pl b v0 sd hd =
  let vf = translateStraight sd <| translateDiagonal (turnDiagonal sd hd) v0
  in Matrix.get vf b
    |> M.join
    |> M.filter (\p -> pl == piecePlayer p && Knight == pieceType p)
    |> Maybe.map (Tile vf >> Tuple.pair (sd, hd))

inKnightCheck : Player -> Board -> V2 -> List ((StraightDirection, HorizontalDirection), Tile)
inKnightCheck pl b v = let check = inKnightOneCheck pl b v in
  [ check N Left
  , check N Right
  , check E Left
  , check E Right
  , check S Left
  , check S Right
  , check W Left
  , check W Right
  ]
  |> List.filterMap identity


inPawnOneCheck : Player -> Board -> V2 -> DiagonalDirection -> Maybe (DiagonalDirection, Tile)
inPawnOneCheck pl b v0 d =
  let vf = translateDiagonal d v0
  in Matrix.get vf b
    |> M.join
    -- |> M.filter (\p -> pl == piecePlayer p && Pawn == pieceType p)
    -- TODO
    |> M.filter ((==) (Piece pl Pawn))
    |> Maybe.map (Tile vf >> Tuple.pair d)

inPawnCheck : Player -> Board -> V2 -> List (DiagonalDirection, Tile)
inPawnCheck pl b v = pl
  |> player [ SE, SW ] [ NE, NW ]
  |> List.map (inPawnOneCheck pl b v)
  |> List.filterMap identity

inCheck : Player -> Board -> V2 -> List Tile
inCheck pl b v =
  Matrix.get v b
  |> M.join
  -- REVIEW
  |> M.filter (piecePlayer >> (==) pl)
  |> M.unwrap
    [ inPawnCheck   pl b v |> List.map Tuple.second
    , inKnightCheck pl b v |> List.map Tuple.second
    , inDiagonalsCheck isDiagonalAttacker pl b v |> List.map Tuple.second
    , inStraightsCheck isStraightAttacker pl b v |> List.map Tuple.second
    , inKingCheck   pl b v |> List.map Tuple.second
    ]
    (always [])
  |> List.concat

isPlayerInCheck : Player -> Board -> Bool
isPlayerInCheck pl b =
  findKing pl b
  |> Maybe.map (inCheck (opponent pl) b >> List.isEmpty)
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
  -- TODO
  -- RegularMoveError extract
  | OutOfBoundsMoveError V2
  | MissingPieceMoveError
  | IncorrectPieceTypeMoveError PieceType
  | IncorrectPlayerMoveError Player

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
type PlayError = PlayError PieceMove Game MoveError

tryMove : PieceMove -> Game -> Result PlayError Game
tryMove m g =
  (case m of
    PawnPieceMove pp -> case pp of
      PawnAdvance v ->
        validateMovePiece v Pawn g
        |> Result.andThen (pawnAdvance v 1 >> Result.mapError PawnAdvancePieceMoveError)
        |> Result.map (Tuple.second >> asBoardIn g)
      PawnDoubleAdvance f ->
        let v = (f, pawnsRank (gameTurn g))
        in validateMovePiece v Pawn g
        |> Result.andThen (pawnAdvance v 2 >> Result.mapError PawnAdvancePieceMoveError)
        |> Result.map (Tuple.second >> asBoardIn g)
      PawnCapture v d ->
        validateMovePiece v Pawn g
        |> Result.andThen (pawnCapture v d >> Result.mapError PawnCapturePieceMoveError)
        |> Result.map (Tuple.second >> asBoardIn g)
      PawnPromotion pr pm ->
        (case pm of 
          PawnPromotionAdvance f -> pawnPromotionAdvance f g
          PawnPromotionCapture f d -> pawnPromotionCapture f d g
        )
        |> Result.mapError PawnPromotionPieceMoveError
        |> Result.andThen
          (\(Translation v0 vf, nb) ->
            validateMovePiece v0 Pawn g
            |> Result.map
            (\_ ->
              Matrix.set vf (Just (Piece (gameTurn g) (promote pr))) nb
              |> asBoardIn g
            )
          )
      PawnEnPassant h ->
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
        >> (\ng ->
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
    QueenPieceMove (QueenMove v d i) ->
      validateMovePiece v Queen g
      |> Result.andThen (queenMove v d i >> Result.mapError QueenMovePieceMoveError)
      |> Result.map (Tuple.second >> asBoardIn g)
    KingPieceMove pp ->
      (case pp of
        KingMove v d ->
          validateMovePiece v King g
          |> Result.andThen (kingMove v d >> Result.mapError KingMovePieceMoveError)
          |> Result.map (Tuple.second >> asBoardIn g)
        KingCastling c ->
          Result.mapError KingCastlingPieceMoveError (kingCastling c g)
          |> Result.map (asBoardIn g)
      )
      |> Result.map
        (\ng -> case gameTurn g of
          White -> { ng | whiteCastlingAvailable = castlingDisabled }
          Black -> { ng | blackCastlingAvailable = castlingDisabled }
        )
  )
  |> Result.map (\ng ->
    { ng
    | moves =
      { move = m
      , board = g.board
      , whiteCastlingAvailable = g.whiteCastlingAvailable
      , blackCastlingAvailable = g.blackCastlingAvailable
      } :: ng.moves
    })
  |> Result.mapError (PlayError m g)

play : List PieceMove -> Game -> Result PlayError Game
play ms g = List.foldr (\m -> Result.andThen (tryMove m)) (Result.Ok g) ms

undoMove : Game -> Maybe (Move, Game)
undoMove s =
  L.uncons s.moves
  |> Maybe.map
  (\(m, ms) ->
    (m
    , { s
      | moves = ms
      , board = m.board
      , whiteCastlingAvailable = m.whiteCastlingAvailable
      , blackCastlingAvailable = m.blackCastlingAvailable
      }
    )
  )

checkDiagonal : Player -> Board -> V2 -> DiagonalDirection -> List (V2, Maybe Piece)
checkDiagonal pl b v0 d =
  let vf = translateDiagonal d v0
  in Matrix.get vf b
    |> M.unwrap
      []
      (\mp -> M.unwrap
        ((vf, mp) :: checkDiagonal pl b vf d)
        (\p -> if pl == piecePlayer p then [] else [ (vf, mp) ])
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
        (\p -> if pl == piecePlayer p then [] else [ (vf, mp) ])
        mp
      )

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

type GameStatus
  = Invalid
  | Normal
  | Check
  | CheckMate
  | StaleMate

gameStatus : Player -> Game -> GameStatus
gameStatus pl g
  = findKing pl g.board
  |> M.unwrap
    Invalid
    (\vk ->
      let kInCheck = inCheck (opponent pl) g.board vk
      in case kInCheck of
        [] -> Normal
        -- One vs More
        cs ->
          let ps = playerPieces pl g.board
              ms = List.concat <| List.map (Tuple2.uncurry <| pieceLegalMoves g) ps
          in if List.isEmpty ms
          then StaleMate
          else
            let klms = kingLegalMoves vk g
            in if List.isEmpty klms
            then case cs of
              c :: [] -> CheckMate -- TODO other ways of escaping Checkmate
              _       -> CheckMate
            else Check
    )

showTile : (Int, Int) -> String
showTile (f, r) = intToAlphabet f ++ String.fromInt (r + 1)
