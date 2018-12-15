module Main (main) where

import           Data.Foldable
import           MazesForProgrammers
import           System.IO.Temp

pngTempFileTemplate :: FilePath
pngTempFileTemplate = "mazes-for-programmers-.png"

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
        mz@(Maze m n _) = case mbMaze of
                Nothing -> error "Invalid maze"
                Just mz' -> mz'
    putStrLn $ mazeAscii mz
    p <- emptySystemTempFile pngTempFileTemplate
    mazePng p mz
    putStrLn "Done"
