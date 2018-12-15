module MazesForProgrammers.Png
    ( mazePng
    ) where

import           Codec.Picture
import           Codec.Picture.Canvas
import           Data.Bool
import           Data.Foldable
import qualified Data.Map.Strict as Map
import           Data.Word
import           MazesForProgrammers.Types
import           System.Process

type Canvas_ = Canvas Word8

drawBox :: Int -> Maze -> Int -> Int -> Canvas_ -> Canvas_
drawBox spacing (Maze m n mp) i j c =
    let sides = Map.findWithDefault [] (i, j) mp
        w = m * spacing
        h = n * spacing
        left = j * spacing
        right = left + spacing - 1
        top = i * spacing
        bottom = top + spacing -1
    in foldl'
            (\c ((x0, y0, x1, y1), side) -> bool (drawLine x0 y0 x1 y1 0 c) c (side `elem` sides))
            c
            [ ((left, top, right, top), N)
            , ((right, top, right, bottom), E)
            , ((right, bottom, left, bottom), S)
            , ((left, bottom, left, top), W)
            ]

drawRow :: Int -> Maze -> Int -> Canvas_ -> Canvas_
drawRow spacing mz@(Maze _ n _) i c =
    foldl'
        (\c' j -> drawBox spacing mz i j c')
        c
        [0..(n - 1)]

mazePng :: Int -> FilePath -> Maze -> IO ()
mazePng spacing path mz@(Maze m n _) = do
    let h = m * spacing
        w = n * spacing
        img = generateImage (\_ _ -> 255) w h
        Right c0 = imageToCanvas img
        c1 = foldl' (\c' i -> drawRow spacing mz i c') c0 [0..(m - 1)]
        img1 = canvasToImage c1
    writePng path img1
    callCommand ("xdg-open " ++ path)
