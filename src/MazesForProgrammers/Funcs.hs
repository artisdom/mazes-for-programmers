module MazesForProgrammers.Funcs
    ( maze
    ) where

import           Data.List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           MazesForProgrammers.Types

updateSideMap :: Int -> Int -> Map Loc [Side] -> Opening -> Map Loc [Side]
updateSideMap m n mp (Opening loc@(i, j) side) =
    let mp' =  Map.insertWith (++) loc [side] mp
    in
        case side of
            N -> if i > 0 then Map.insertWith (++) (i - 1, j) [S] mp' else mp'
            E -> if j < n - 1 then Map.insertWith (++) (i, j + 1) [W] mp' else mp'
            S -> if i < m - 1 then Map.insertWith (++) (i + 1, j) [N] mp' else mp'
            W -> if j > 0 then Map.insertWith (++) (i, j - 1) [E] mp' else mp'

maze :: Int -> Int -> [Opening] -> Maze
maze m n os = Maze m n (foldl' (updateSideMap m n) Map.empty os)
