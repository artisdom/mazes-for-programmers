module MazesForProgrammers.Ascii (mazeAscii) where

import           Data.Bool
import           Data.List
import qualified Data.Map.Strict as Map
import           MazesForProgrammers.Types

cellAscii :: Int -> Int -> Maze -> (String, String)
cellAscii i j (Maze _ _ mp) =
    let sides = Map.findWithDefault [] (i, j) mp
        sideE : sideS : [] = map (`elem` sides) [E, S]
    in
        ( bool "   |" "    " sideE
        , bool "---+" "   +" sideS
        )

rowAscii :: Int -> Maze -> (String, String)
rowAscii i maze@(Maze _ n _ ) =
    foldl'
        (\(l0, l1) j ->
            let (s0, s1) = cellAscii i j maze
            in (l0 ++ s0, l1 ++ s1))
        ("|", "+")
        [0..(n - 1)]

mazeAscii :: Maze -> String
mazeAscii maze@(Maze m n _) =
    intercalate "\n" $
        (concat (take n (repeat "+---")) ++ "+") :
        foldl'
            (\lines i -> let (a, b) = rowAscii i maze in lines ++ [a, b])
            []
            [0..(m - 1)]
