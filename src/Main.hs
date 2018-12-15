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
    mazePng 100 p mz
    putStrLn "Done"

demoBinaryTreeMaze :: IO ()
demoBinaryTreeMaze = do
    let m = 50
        n = 50

    -- This is the binary tree algorithm
    -- It all runs in IO which sucks
    -- I'll fix this later
    openings <- foldlM (\os (i, j) -> do
        let ns0 = []
            ns1 = bool ns0 (Opening (i, j) N : ns0) (i > 0)
            ns2 = bool ns1 (Opening (i, j) W : ns1) (j > 0)
        mbOpening <- pickElement ns2
        case mbOpening of
            Nothing -> pure os
            Just opening -> pure $ opening : os)
        []
        [(i, j) | i <- [0..(m - 1)], j <- [0..(n - 1)]]

    let mbMaze = foldlM withOpening (maze m n) openings
        mz = case mbMaze of
            Nothing -> error "Invalid maze"
            Just mz' -> mz'

    p <- emptySystemTempFile pngTempFileTemplate
    mazePng 20 p mz
    putStrLn "Done"

pickElement :: [a] -> IO (Maybe a)
pickElement xs = do
    let count = length xs
    if count > 0
        then do
            idx <- randomRIO (0, count - 1)
            pure (Just $ xs !! idx)
        else pure Nothing

main :: IO ()
main = demoBinaryTreeMaze
