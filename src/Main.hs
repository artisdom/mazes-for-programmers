module Main (main) where

import           Data.Foldable
import           MazesForProgrammers

main :: IO ()
main = do
    let mbMaze = foldlM withOpening (maze 5 4)
            [ Opening (0, 0) E
            , Opening (0, 1) E
            , Opening (0, 2) E
            , Opening (0, 3) S
            , Opening (1, 3) W
            , Opening (2, 2) N
            , Opening (4, 0) N
            ]
    print mbMaze
    case mbMaze of
        Nothing -> putStrLn "Invalid maze"
        Just m -> putStrLn $ mazeAscii m
