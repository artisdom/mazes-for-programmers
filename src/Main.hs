module Main (main) where

import           MazesForProgrammers

main :: IO ()
main = do
    let m = maze 5 4
            [ Opening (0, 0) E
            , Opening (0, 1) E
            , Opening (0, 2) E
            , Opening (0, 3) S
            , Opening (1, 3) W
            , Opening (2, 2) N
            , Opening (4, 0) N
            , Opening (0, 0) N
            ]
    print m
    case m of
        Nothing -> putStrLn "Invalid maze"
        Just m' -> putStrLn $ mazeAscii m'
