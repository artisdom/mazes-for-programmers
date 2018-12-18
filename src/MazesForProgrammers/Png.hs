module MazesForProgrammers.Png
    ( mazePng
    ) where

import           Codec.Picture
import           Codec.Picture.Types
import           Control.Monad
import           Control.Monad.Primitive
import           Data.Bool
import           Data.Foldable
import qualified Data.Map.Strict as Map
import           Data.Word
import           MazesForProgrammers.Types
import           MazesForProgrammers.Util

type Point2D = (Int, Int)

withMutableImage :: (Pixel px, PrimMonad m) => Int -> Int -> px -> (MutableImage (PrimState m) px -> m ()) -> m (Image px)
withMutableImage w h px f = do
    m <- createMutableImage w h px
    f m
    unsafeFreezeImage m

green :: Double -> PixelRGB8
green a =
    let f = scale a; r = 0; g = 255; b = 0
    in PixelRGB8 (f r) (f g) (f b)

scale :: Double -> Pixel8 -> Pixel8
scale a x = round (a * fromIntegral x)

mazePng :: Int -> FilePath -> Maze -> IO ()
mazePng spacing path mz@(Maze m n mp) = do
    let w = m * spacing
        h = n * spacing
    img <- withMutableImage w h (PixelRGB8 255 255 255) $ \mimg -> do
        for_ [(i, j) | i <- [0..(m - 1)], j <- [0..(n - 1)]] $ \(i, j) -> do
            let sides = Map.findWithDefault [] (i, j) mp
                top = i * spacing
                left = j * spacing
                bottom = top + spacing - 1
                right = left + spacing - 1
            fillRectangle mimg (left, top) (right, bottom) (green ((fromIntegral $ i * j) / (fromIntegral $ m * n)))
            for_
                [ (N, drawHLine mimg left top right)
                , (E, drawVLine mimg right top bottom)
                , (S, drawHLine mimg right bottom left)
                , (W, drawVLine mimg left bottom top)
                ] $ \(side, action) -> unless (side `elem` sides) (action $ PixelRGB8 255 255 255)

    writePng path img
    shellOpen path

drawHLine :: (Pixel px, PrimMonad m) => MutableImage (PrimState m) px -> Int -> Int -> Int -> px -> m ()
drawHLine m x0 y x1 px
    | x1 > x0 = for_ [x0..x1] $ \x -> writePixel m x y px
    | otherwise = for_ [x0, (x0 - 1)..x1] $ \x -> writePixel m x y px

drawVLine :: (Pixel px, PrimMonad m) => MutableImage (PrimState m) px -> Int -> Int -> Int -> px -> m ()
drawVLine m x y0 y1 px
    | y1 > y0 = for_ [y0..y1] $ \y -> writePixel m x y px
    | otherwise = for_ [y0, (y0 - 1)..y1] $ \y -> writePixel m x y px

fillRectangle :: (Pixel px, PrimMonad m) => MutableImage (PrimState m) px -> Point2D -> Point2D -> px -> m ()
fillRectangle m (x0, y0) (x1, y1) px =
    for_ [(x, y) | x <- [x0..x1], y <- [y0..y1]] $ \(x, y) -> writePixel m x y px

fillTriangle :: (Pixel px, PrimMonad m) => MutableImage (PrimState m) px -> Int -> Int -> Point2D -> Point2D -> Point2D -> px -> m ()
fillTriangle m w h v0@(v0x, v0y) v1@(v1x, v1y) v2@(v2x, v2y) px =
    let (minX, maxX) = minMax3 v0x v1x v2x
        (minY, maxY) = minMax3 v0y v1y v2y
        minX' = max minX 0
        minY' = max minY 0
        maxX' = min maxX (w - 1)
        maxY' = min maxY (h - 1)
    in
        for_ [(x, y) | x <- [minX'..maxX'], y <- [minY'..maxY']] $ \p@(x, y) -> do
            let w0 = orient2D v1 v2 p
                w1 = orient2D v2 v0 p
                w2 = orient2D v0 v1 p
            when (w0 >= 0 && w1 >= 0 && w2 >= 0) $ writePixel m x y px

orient2D :: Point2D -> Point2D -> Point2D -> Int
orient2D (ax, ay) (bx, by) (cx, cy) = (bx - ax) * (cy - ay) - (by - ay) * (cx - ax)

min3 :: Int -> Int -> Int -> Int
min3 a b c
    | a < b = min a c
    | otherwise = min b c

max3 :: Int -> Int -> Int -> Int
max3 a b c
    | a > b = max a c
    | otherwise = max b c

minMax3 :: Int -> Int -> Int -> (Int, Int)
minMax3 a b c = (min3 a b c, max3 a b c)
