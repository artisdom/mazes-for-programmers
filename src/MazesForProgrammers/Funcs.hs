module MazesForProgrammers.Funcs
    ( maze
    ) where

import           Data.Foldable
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           MazesForProgrammers.Types

updateSideMap :: Int -> Int -> Map Loc [Side] -> Opening -> Maybe (Map Loc [Side])
updateSideMap m n mp (Opening loc@(i, j) side) =
    let mp' =  Map.insertWith (++) loc [side] mp
    in
        case side of
            N -> if i > 0 then Just (Map.insertWith (++) (i - 1, j) [S] mp') else Nothing
            E -> if j < n - 1 then Just (Map.insertWith (++) (i, j + 1) [W] mp') else Nothing
            S -> if i < m - 1 then Just (Map.insertWith (++) (i + 1, j) [N] mp') else Nothing
            W -> if j > 0 then Just (Map.insertWith (++) (i, j - 1) [E] mp') else Nothing

maze :: Int -> Int -> [Opening] -> Maybe Maze
maze m n os = Maze m n <$> (foldlM (updateSideMap m n) Map.empty os)
