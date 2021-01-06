module Chess.Board exposing (..)

import Matrix exposing (..)
import Chess.Base exposing (..)

composeBoard : Board -> List Tile -> Board
composeBoard = List.foldl (\(Tile (x, y) p) g -> Matrix.set (x, y) (Just p) g)
