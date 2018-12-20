module MazesForProgrammers.Png
    ( mazePng
    ) where

import           Codec.Picture
import           Codec.Picture.Drawing
import           Codec.Picture.Types
import           Control.Monad
import           Control.Monad.Primitive
import           Data.Bool
import           Data.Foldable
import qualified Data.Map.Strict as Map
import           Data.Word
import           MazesForProgrammers.Types
import           MazesForProgrammers.Util

mkGreen :: Double -> PixelRGB8
mkGreen a =
    let f = scale a; r = 0; g = 255; b = 0
    in PixelRGB8 (f r) (f g) (f b)

white :: PixelRGB8
white = PixelRGB8 255 255 255

scale :: Double -> Pixel8 -> Pixel8
scale a x = round (a * fromIntegral x)

mazePng :: Int -> FilePath -> Maze -> IO ()
mazePng spacing path mz@(Maze m n mp) = do
    let w = m * spacing
        h = n * spacing
    img <- withDefaultMutableImage w h $ \mimg -> do
        for_ [(i, j) | i <- [0..(m - 1)], j <- [0..(n - 1)]] $ \(i, j) -> do
            let sides = Map.findWithDefault [] (i, j) mp
                top = i * spacing
                left = j * spacing
                bottom = top + spacing - 1
                right = left + spacing - 1
            fillRectangle mimg left top right bottom (mkGreen ((fromIntegral $ i * j) / (fromIntegral $ m * n)))
            for_
                [ (N, drawLine mimg left top right top white)
                , (E, drawLine mimg right top right bottom white)
                , (S, drawLine mimg right bottom left bottom white)
                , (W, drawLine mimg left bottom left top white)
                ] $ \(side, action) -> unless (side `elem` sides) action

    writePng path img
    shellOpen path
