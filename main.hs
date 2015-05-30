{-# LANGUAGE TypeOperators #-}

module Main where

import Data.Array.Repa.IO.DevIL
import Data.Array.Repa (Array, slice, Source)
import Data.Array.Repa.Eval
import Data.Array.Repa.Index
import Data.Array.Repa.Slice
import Data.Word
import Control.Applicative ()
import qualified Data.Array.Repa as R


imgChannel :: (Source r w, Target r w) => Int -> Array r DIM3 w -> Array r DIM2 w
imgChannel n img = R.computeS $ slice img (Z :. All :. All :. n)

red, green, blue, alpha :: Image -> Image

red img = case img of
            RGBA a -> Grey . imgChannel 0 $ a
            RGB a -> Grey . imgChannel 0 $ a
            BGRA a -> Grey . imgChannel 2 $ a
            BGR a -> Grey . imgChannel 2 $ a
            Grey _ -> error "no red channel in Grey images."

green img = case img of
            RGBA a -> Grey . imgChannel 1 $ a
            RGB a -> Grey . imgChannel 1 $ a
            BGRA a -> Grey . imgChannel 1 $ a
            BGR a -> Grey . imgChannel 1 $ a
            Grey _ -> error "no green channel in Grey images."

blue img = case img of
            RGBA a -> Grey . imgChannel 2 $ a
            RGB a -> Grey . imgChannel 2 $ a
            BGRA a -> Grey . imgChannel 0 $ a
            BGR a -> Grey . imgChannel 0 $ a
            Grey _ -> error "no blue channel in Grey images."

alpha img = case img of
            RGBA a -> Grey . imgChannel 3 $ a
            BGRA a -> Grey . imgChannel 3 $ a
            RGB _ -> error "no alpha channel in RGB images."
            BGR _ -> error "no alpha channel in BGR images."
            Grey _ -> error "no alpha channel in Grey images."

mapS :: (Source r w, Target r w', R.Shape d) => (w -> w') -> Array r d w -> Array r d w'
mapS f = R.computeS . R.map f

mapP :: (Load r d w, Source r w', Target r w', R.Shape d, Monad m) => (w -> w') -> Array r d w -> m (Array r d w')
mapP f = R.computeP . R.map f

-- image mapping only really defined for Word8s
-- otherwise we care about what kind of image this is.
imageMapS :: (Word8 -> Word8) -> Image -> Image
imageMapS f (RGBA img) = RGBA $ mapS f img
imageMapS f (RGB img) = RGB $ mapS f img
imageMapS f (BGRA img) = BGRA $ mapS f img
imageMapS f (BGR img) = BGR $ mapS f img
imageMapS f (Grey img) = Grey $ mapS f img

imageMapP :: (Word8 -> Word8) -> Image -> Image
imageMapP f (RGBA img) = RGBA $ mapS f img
imageMapP f (RGB img) = RGB $ mapS f img
imageMapP f (BGRA img) = BGRA $ mapS f img
imageMapP f (BGR img) = BGR $ mapS f img
imageMapP f (Grey img) = Grey $ mapS f img


sigmoid :: Double -> Double -> Double
sigmoid y x = (2*x)**y / ((1 + x)**y)

liftW8 :: (Double -> Double) -> Word8 -> Word8
liftW8 f = word8FromDouble . f . word8ToDouble
    where
        word8ToDouble w = fromIntegral w / 256.0
        word8FromDouble = fromIntegral . (\w -> if w > 255 then 255 else w :: Int) . round . (*) 256.0 

main :: IO ()
main = runIL $ do
        let f = "/Users/cspollard/Pictures/Photos/External/HalfChris.jpg"
        -- r <- red <$> readImage f
        r <- readImage f
        writeImage "/Users/cspollard/Desktop/red.JPG" r
        writeImage "/Users/cspollard/Desktop/sigmoidred.JPG" . imageMapS (liftW8 (sigmoid 4.0)) $ r
        writeImage "/Users/cspollard/Desktop/slidered.JPG" . imageMapS (+ 12) $ r
        writeImage "/Users/cspollard/Desktop/mulred.JPG" . imageMapS (liftW8 (* 1.5)) $ r
