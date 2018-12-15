module MazesForProgrammers.Funcs
    ( maze
    ) where

import           Data.Foldable
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           MazesForProgrammers.Types

updateSideMap :: Int -> Int -> Map Loc [Side] -> Opening -> Maybe (Map Loc [Side])
updateSideMap m n mp (Opening loc@(i, j) side)
    | side == N && i > 0 = Just (Map.insertWith (++) (i - 1, j) [S] mp')
    | side == E && j < n - 1 = Just (Map.insertWith (++) (i, j + 1) [W] mp')
    | side == S && i < m - 1 = Just (Map.insertWith (++) (i + 1, j) [N] mp')
    | side == W && j > 0 = Just (Map.insertWith (++) (i, j - 1) [E] mp')
    | otherwise = Nothing
    where mp' =  Map.insertWith (++) loc [side] mp

maze :: Int -> Int -> [Opening] -> Maybe Maze
maze m n os = Maze m n <$> (foldlM (updateSideMap m n) Map.empty os)
