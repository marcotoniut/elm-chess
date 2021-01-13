module Chess.Board exposing (..)

import Chess.Base exposing (..)
import Matrix exposing (..)

composeBoard : Board -> List Tile -> Board
composeBoard = List.foldl (\(Tile (x, y) p) g -> Matrix.set (x, y) (Just p) g)
