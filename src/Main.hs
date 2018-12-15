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
    g <- getStdGen

    let (openings, _) = doBinaryTree m n g
        mbMaze = foldlM withOpening (maze m n) openings
        mz = case mbMaze of
            Nothing -> error "Invalid maze"
            Just mz' -> mz'

    p <- emptySystemTempFile pngTempFileTemplate
    mazePng 20 p mz
    putStrLn "Done"

doBinaryTree :: RandomGen g => Int -> Int -> g -> ([Opening], g)
doBinaryTree m n g =
    foldl'
        (\(os, g') (i, j) ->
            let ns0 = []
                ns1 = bool ns0 (Opening (i, j) N : ns0) (i > 0)
                ns2 = bool ns1 (Opening (i, j) W : ns1) (j > 0)
                (mbOpening, g'') = pickElement ns2 g'
            in case mbOpening of
                Nothing -> (os, g'')
                Just opening -> (opening : os, g''))
        ([], g)
        [(i, j) | i <- [0..(m - 1)], j <- [0..(n - 1)]]

pickElement :: RandomGen g => [a] -> g -> (Maybe a, g)
pickElement xs g =
    let count = length xs
    in if count > 0
        then
            let (idx, g') = randomR (0, count - 1) g
            in (Just $ xs !! idx, g')
        else (Nothing, g)

main :: IO ()
main = demoBinaryTreeMaze
