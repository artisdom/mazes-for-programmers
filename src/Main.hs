module Main (main) where

import           Data.Bool
import           Data.Foldable
import           MazesForProgrammers
import           System.IO.Temp
import           System.Random

pngTempFileTemplate :: FilePath
pngTempFileTemplate = "mazes-for-programmers-.png"

demoHandwrittenMaze :: IO ()
demoHandwrittenMaze = do
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

demoBinaryTreeMaze :: IO ()
demoBinaryTreeMaze = do
    let mz@(Maze m n _) = maze 5 4

    -- This is the binary tree algorithm
    -- It all runs in IO which sucks
    -- I'll fix this later
    openings <- foldlM (\os (i, j) -> do
        let ns0 = []
            ns1 = bool ns0 (Opening (i, j) N : ns0) (i > 0)
            ns2 = bool ns1 (Opening (i, j) W : ns1) (j > 0)
            count = length ns2
        if count > 0
            then do
                idx <- randomRIO (0, count - 1)
                let neighbour = ns2 !! idx
                pure $ neighbour : os
            else pure os)
        []
        [(i, j) | i <- [0..(m - 1)], j <- [0..(n - 1)]]

    let mbResult = foldlM withOpening mz openings
        result = case mbResult of
            Nothing -> error "Invalid maze"
            Just mz' -> mz'

    p <- emptySystemTempFile pngTempFileTemplate
    mazePng p result
    putStrLn "Done"

main :: IO ()
main = demoBinaryTreeMaze
