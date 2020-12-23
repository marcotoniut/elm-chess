module Chess.AlgebraicNotation exposing (..)

import Alphabet exposing (intToAlphabet)
import Chess exposing (..)
import Direction exposing (..)
import List.Extra as L
import Maybe.Extra as M
import Result.Extra as R
import Matrix
import Maybe
import Alphabet exposing (intFromAlphabetChar)

type ToANError
  = ToANPlayError PlayError

pieceFragment : PieceType -> String
pieceFragment p = case p of
  Pawn   -> ""
  Knight -> "N"
  Bishop -> "B"
  Rook   -> "R"
  Queen  -> "Q"
  King   -> "K"

pieceFragmentFromChar : Char -> Maybe PieceType
pieceFragmentFromChar c = case c of
  'N' -> Just Knight
  'B' -> Just Bishop
  'R' -> Just Rook
  'Q' -> Just Queen
  'K' -> Just King
  _   -> Nothing

captureFragmentChar : Char
captureFragmentChar = 'x'

captureFragment : String
captureFragment = String.fromChar captureFragmentChar

showCaptured : V2 -> Matrix.Matrix (Maybe a) -> String
showCaptured v b = Matrix.get v b |> M.join |> M.unwrap "" (always captureFragment)

-- TODO Based on ambiguity, use only rank or file
toAN : PieceMove -> Game -> Result ToANError String
toAN m g =
  let pl = gameTurn g
  in tryMove m g
  |> Result.mapError ToANPlayError
  |> Result.andThen
    (\ng -> case m of
      PawnPieceMove pm -> case pm of
        PawnAdvance v -> Result.Ok <| showTile <| pawnAdvanceTarget pl v
        PawnDoubleAdvance i -> Result.Ok <| showTile <| pawnDoubleAdvanceTarget pl i
        PawnCapture v h ->
          let vf = pawnCaptureTarget pl v h
              dd = turnDiagonal (playerDirection pl) h
              pc = inPawnOneCheck (opponent pl) g.board vf dd
          in if M.isJust pc
          then Result.Ok <| showTile v ++ captureFragment ++ showTile vf
          else Result.Ok <| captureFragment ++ showTile vf
        PawnEnPassant h ->
          case List.head g.moves of
            Nothing -> Result.Err (ToANPlayError (PlayError m g (PawnEnPassantPieceMoveError PawnEnPassantUnavailable)))
            Just  l -> case l.move of
              PawnPieceMove (PawnDoubleAdvance f) ->
                Result.Ok
                <| intToAlphabet f
                ++ captureFragment
                ++ showTile (pawnEnPassantTarget pl f h)
                ++ " e.p."
              _ ->  Result.Err (ToANPlayError (PlayError m g (PawnEnPassantPieceMoveError PawnEnPassantUnavailable)))
        PawnPromotion pr ppm ->
          Result.Ok <| 
          (case ppm of
            PawnPromotionAdvance f -> showTile (f, promotionRank pl)
            PawnPromotionCapture f h ->
              let v0 = (translateHorizontal h f, promotionRank pl)
                  vf = pawnCaptureTarget pl v0 h
                  dd = turnDiagonal (playerDirection pl) h
                  pc = inPawnOneCheck pl g.board vf dd
              in if M.isJust pc
              then showTile v0 ++ captureFragment ++ showTile vf
              else captureFragment ++ showTile vf
          ) ++ pieceFragment (promote pr)
      KnightPieceMove (KnightMove v0 sd hd) ->
        let vf = knightMoveTarget v0 sd hd
        in Result.Ok
          <| pieceFragment Knight
          ++ (if List.isEmpty (inKnightCheck (opponent pl) g.board vf) then "" else showTile v0)
          ++ showCaptured vf g.board
          ++ showTile vf
      BishopPieceMove (BishopMove v0 d i) ->
        let vf = bishopMoveTarget i d v0
        in Result.Ok
          <| pieceFragment Bishop
          ++ (if List.isEmpty (inDiagonalsCheck ((==) Bishop) (opponent pl) g.board vf) then "" else showTile v0)
          ++ showCaptured vf g.board
          ++ showTile vf
      RookPieceMove (RookMove v0 d i) ->
        let vf = rookMoveTarget i d v0
        in Result.Ok
          <| pieceFragment Rook
          ++ (if List.isEmpty (inStraightsCheck ((==) Rook) (opponent pl) g.board vf) then "" else showTile v0)
          ++ showCaptured vf g.board
          ++ showTile vf
      QueenPieceMove (QueenMove v0 d i) ->
        let vf = queenMoveTarget i d v0
            qc = inQueenCheck (opponent pl) g.board vf
        in Result.Ok
          <| pieceFragment Queen
          ++ (if List.isEmpty qc then "" else showTile v0)
          ++ showCaptured vf g.board
          ++ showTile vf
      KingPieceMove   km ->
        case km of
          KingMove v d ->
            let vf = kingMoveTarget d v
            in Result.Ok <| pieceFragment King ++ showCaptured vf g.board ++ showTile vf
          KingCastling c ->
            Result.Ok <| case c of
              QueenSide -> "O-O-O"
              KingSide  -> "O-O"
    )

gameAN : Game -> Result ToANError (List String)
gameAN = undoMove >> M.unwrap (Result.Ok []) (\(m, g) -> R.andMap (gameAN g) (Result.map (::) (toAN m.move g)))

-- TODO Expand ANInvalid
type ANError = ANInvalid | ANAmbiguousMove | ANDisambiguationUnnecessary

parseAN : Player -> Board -> String -> Result ANError PieceMove
parseAN pl b an = case an of
  "O-O"   -> Result.Ok <| KingPieceMove <| KingCastling KingSide
  "O-O-O" -> Result.Ok <| KingPieceMove <| KingCastling QueenSide
  -- TODO En Passant
  an0 ->
    case String.uncons (String.reverse an0) of
      Nothing -> Result.Err ANInvalid
      Just (c, an1) -> case pieceFragmentFromChar c |> Maybe.andThen promotionFromPieceType of
        Just pr ->
          pluckTileReverseAN an1
          |> Maybe.andThen
            (\((ff, rf), an2) ->
              if 0 == String.length an2
              then Just <| PawnPieceMove <| PawnPromotion pr <| PawnPromotionAdvance ff
              else String.uncons an2
                |> M.filter (Tuple.first >> (==) captureFragmentChar)
                |> Maybe.andThen (Tuple.second >> String.uncons)
                |> M.filter (Tuple.second >> (==) "")
                |> Maybe.andThen (Tuple.first >> intFromAlphabetChar)
                |> Maybe.andThen
                  (\f0 ->
                    if f0 - 1 == ff
                    then Just (f0, Right)
                    else if f0 + 1 == ff
                    then Just (f0, Left)
                    else Nothing
                  )
                |> Maybe.map
                (\(f0, hd) -> PawnPieceMove <| PawnPromotion pr <| PawnPromotionCapture f0 hd)
            )
          |> Result.fromMaybe ANInvalid
        Nothing ->
          pluckTileReverseAN (String.reverse an0)
          |> Maybe.andThen
            (\(vf, an2) ->
              let (ff, rf) = vf
              in String.uncons an2
              |> M.unwrap
                (Just <| PawnPieceMove <| PawnAdvance <| translateStraight (player S N pl) vf)
                (\(cx, an3) ->
                  let capture = cx == captureFragmentChar
                      an4 = String.reverse (if capture then an3 else an2)
                  in String.uncons an4
                    |> Maybe.andThen
                      (\(pc, an5) ->
                        let l5 = String.length an5
                        in if l5 /= 0 && l5 /= 2
                        then Nothing
                        else pieceFragmentFromChar pc
                        |> Maybe.andThen
                          (\p ->
                            if l5 == 0
                            then case p of
                              Knight -> case inKnightCheck pl b vf of
                                  -- CHECK PieceType ?
                                ((sd, hd), Tile v0 pt) :: [] -> Just <| KnightPieceMove <| KnightMove v0 (oppositeStraight sd) hd
                                _ -> Nothing
                              Bishop -> case inBishopCheck pl b vf of
                                ((d, n),   Tile v0 pt) :: [] -> Just <| BishopPieceMove <| BishopMove v0 (oppositeDiagonal d) n
                                _ -> Nothing
                              Rook   -> case inRookCheck pl b vf of
                                ((d, n),   Tile v0 pt) :: [] -> Just <| RookPieceMove   <| RookMove   v0 (oppositeStraight d) n
                                _ -> Nothing
                              Queen  -> case inQueenCheck pl b vf of
                                ((d, n),   Tile v0 pt) :: [] -> Just <| QueenPieceMove  <| QueenMove  v0 (opposite d) n
                                _ -> Nothing
                              King   -> case inKingCheck pl b vf of
                                (d,        Tile v0 pt) :: [] -> Just <| KingPieceMove   <| KingMove   v0 (opposite d)
                                _ -> Nothing
                              _ -> Nothing
                            else pluckTileAN an5
                              |> Maybe.map Tuple.first
                              |> Maybe.andThen
                                (\v0 -> case p of
                                  Knight ->
                                    case inKnightCheck pl b vf of
                                      []      -> Nothing
                                      _ :: [] -> Nothing -- DisambiguationUnnecessary
                                      xs      ->
                                        L.find (Tuple.second >> (==) (Tile v0 (Piece pl Knight))) xs
                                        |> Maybe.map (Tuple.first >> (\(sd, hd) -> KnightPieceMove <| KnightMove v0 (oppositeStraight sd) hd))
                                  Bishop ->
                                    case inBishopCheck pl b vf of
                                      []      -> Nothing
                                      _ :: [] -> Nothing -- DisambiguationUnnecessary
                                      xs      ->
                                        L.find (Tuple.second >> (==) (Tile v0 (Piece pl Bishop))) xs
                                        |> Maybe.map (Tuple.first >> (\(d, n) -> BishopPieceMove <| BishopMove v0 (oppositeDiagonal d) n))
                                  Rook   ->
                                    case inRookCheck pl b vf of
                                      []      -> Nothing
                                      _ :: [] -> Nothing -- DisambiguationUnnecessary
                                      xs      ->
                                        L.find (Tuple.second >> (==) (Tile v0 (Piece pl Rook))) xs
                                        |> Maybe.map (Tuple.first >> (\(d, n) -> RookPieceMove <| RookMove v0 (oppositeStraight d) n))
                                  Queen  ->
                                    case inQueenCheck pl b vf of
                                      []      -> Nothing
                                      _ :: [] -> Nothing -- DisambiguationUnnecessary
                                      xs      ->
                                        L.find (Tuple.second >> (==) (Tile v0 (Piece pl Queen))) xs
                                        |> Maybe.map (Tuple.first >> (\(d, n) -> QueenPieceMove <| QueenMove v0 (opposite d) n))
                                  _ -> Nothing
                                )
                          )
                      )
                )
            )
          |> Result.fromMaybe ANInvalid

pluckTileAN : String -> Maybe (V2, String)
pluckTileAN = String.uncons
  >> Maybe.andThen
    (\(fc, s) -> String.uncons s
    |> Maybe.andThen
      (\(rc, u) -> rc
      |> String.fromChar
      |> String.toInt
      |> Maybe.andThen
        (\r -> intFromAlphabetChar fc
        |> Maybe.map (\f -> ((f, r - 1), u))
        )
      )
    )

-- REVIEW
pluckTileReverseAN : String -> Maybe (V2, String)
pluckTileReverseAN = String.uncons
  >> Maybe.andThen
    (\(rc, s) -> String.uncons s
    |> Maybe.andThen
      (\(fc, u) -> rc
      |> String.fromChar
      |> String.toInt
      |> Maybe.andThen
        (\r -> intFromAlphabetChar fc
        |> Maybe.map (\f -> ((f, r - 1), u))
        )
      )
    )