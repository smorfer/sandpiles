{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Main where

import           Codec.Picture
import           Control.DeepSeq
import           Data.Array.Repa
import           Data.Array.Repa.Stencil
import           Data.Array.Repa.Stencil.Dim2
import qualified Data.Vector.Unboxed           as V
import           Debug.Trace                   as D
import qualified GHC.Base                      as B
import           Prelude                 hiding ( map )

import           Control.Stabilization
import           Data.Sandpile

pileToColor :: Int -> PixelRGB8
pileToColor = \case
  0 -> PixelRGB8 0 0 0
  1 -> PixelRGB8 255 0 0
  2 -> PixelRGB8 0 255 255
  3 -> PixelRGB8 0 0 255
  n -> PixelRGB8 255 255 255

pileToGreyScale :: Int -> PixelRGB8
pileToGreyScale = \case
  0 -> PixelRGB8 0 0 0
  1 -> PixelRGB8 32 32 32
  2 -> PixelRGB8 64 64 64
  3 -> PixelRGB8 128 128 128
  n -> PixelRGB8 255 255 255

-- I = R(S - R(S))
main :: IO ()
main = do
  putStrLn "Generating pile"
  let !filledWith6 = filledPile 6 128
  putStrLn "Stabilizing filled pile"
  stabilized <- stabilizeWithSteps filledWith6
  putStrLn "Subtracting piles"
  subtracted <- (\pile -> Pile pile (pileSize filledWith6))
    <$> computeUnboxedP (toArray filledWith6 -^ toArray stabilized)
  putStrLn "Stabilizing identity"
  identity <- stabilizeWithSteps subtracted
  putStrLn "Converting to pixels"
  let !images =
        (\m -> generateImage (generator pileToGreyScale m)
                             (pileSize stabilized)
                             (pileSize stabilized)
          )
          (toArray identity)
  putStrLn "Writing image to disk"
  writeBitmap
    ("computed_sandpiles/toppled-" B.++ show (pileSize stabilized) B.++ ".bmp")
    images
  putStrLn "Task done"
 where
  generator :: (Pixel px) => (Int -> px) -> Array U DIM2 Int -> Int -> Int -> px
  generator g !m !x !y = g $ m ! (Z :. x :. y)

