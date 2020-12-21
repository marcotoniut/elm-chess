module Chess.AlgebraicNotation exposing (..)

import Alphabet exposing (intToAlphabet)
import Chess exposing (..)
import Direction exposing (..)
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
            qc = inQueensCheck (opponent pl) g.board vf
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

type ANError = InvalidAN | ANAmbiguousMove

parseAN : Player -> Board -> String -> Result ANError PieceMove
parseAN pl b an = case an of
  "O-O"   -> Result.Ok <| KingPieceMove <| KingCastling KingSide
  "O-O-O" -> Result.Ok <| KingPieceMove <| KingCastling QueenSide
  d ->
    let q  = String.reverse d
        mc = String.uncons q
    in case mc of
      Nothing -> Result.Err InvalidAN
      Just (c, s) -> case pieceFragmentFromChar c |> Maybe.andThen promotionFromPieceType of
        Just pr -> pluckTileReverseAN s
          |> Maybe.andThen
            (\((ff, rf), t) ->
              if 0 == String.length t
              then Just <| PawnPieceMove <| PawnPromotion pr <| PawnPromotionAdvance ff
              else String.uncons t
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
          |> Result.fromMaybe InvalidAN
        Nothing -> Result.Err InvalidAN
          -- let intFromAlphabetChar r


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
            |> Maybe.map (\f -> ((f, r), u))
          )
      )
    )
