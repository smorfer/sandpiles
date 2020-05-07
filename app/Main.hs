{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Main where

import           Codec.Picture
import           Control.DeepSeq
import           Data.Array.Repa
import           Data.Array.Repa.Stencil
import           Data.Array.Repa.Stencil.Dim2
import qualified Data.Vector.Unboxed          as V
import           Debug.Trace                  as D
import qualified GHC.Base                     as B
import           Prelude                      hiding (map)

import           Control.Stabilization
import           Data.Sandpile

pileToColor :: Int -> PixelRGB8
pileToColor =
  \case
    0 -> PixelRGB8 0 0 0
    1 -> PixelRGB8 255 0 0
    2 -> PixelRGB8 0 255 255
    3 -> PixelRGB8 0 0 255
    n -> PixelRGB8 255 255 255

pileToGreyScale :: Int -> PixelRGB8
pileToGreyScale =
  \case
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
  stabilized <- stabilize filledWith6
  putStrLn "Subtracting piles"
  subtracted <-
    (\pile -> Pile pile (pileSize filledWith6)) <$>
    (computeUnboxedP $ (toArray filledWith6) -^ (toArray stabilized))
  putStrLn "Stabilizing identity"
  identity <- stabilize subtracted
  putStrLn "Converting to pixels"
  let !images =
        (\m ->
           generateImage
             (generator pileToGreyScale m)
             (pileSize stabilized)
             (pileSize stabilized))
          (toArray identity)
  putStrLn "Writing image to disk"
  writeBitmap
    ("computed_sandpiles/toppled-" B.++ (show $ pileSize stabilized) B.++ ".bmp")
    images
  putStrLn "Task done"
  where
    generator ::
         (Pixel px) => (Int -> px) -> Array U DIM2 Int -> Int -> Int -> px
    generator g !m !x !y = g $ m ! (Z :. x :. y)

main' :: IO ()
main' = do
  let size = 512
      isIdent = True
      !at = Z :. (size :: Int) :. (size :: Int)
      !sixes = fromListUnboxed at (take (size * size) $ repeat 6 :: [Int])
      boundarySize = (size ^ 2 - 1) `div` 2
      -- !singlePile  = fromListUnboxed
        -- at
        -- ((    (take boundarySize (repeat 0))
         -- B.++ [100000]
         -- B.++ (take boundarySize (repeat 0))
         -- ) :: [Int]
        -- )
      -- !imagesSubtracted =
        -- (\m -> generateImage (generator pileToGreyScale m) size size) computed
      -- !write = writeGifAnimation ("toppling-" B.++ show size B.++ ".gif")
         --                        1
         --                        LoopingForever
         --                        images
  --case write of
    --Left  str    -> putStrLn str
    --Right action -> putStrLn "Saved Gif" >> action
  -- writeBitmap
    -- ("computed_sandpiles/toppled-subtract-" B.++ (show size) B.++ "w5.bmp")
    -- imagesSubtracted
  putStrLn "Wrote Bitmap to disk"
  where
    topple :: Int -> Array U DIM2 Int -> IO (Array U DIM2 Int)
    topple count a = do
      let !st =
            [stencil2|  0 1 0
                          1 0 1
                          0 1 0 |]
          !bin =
            map
              (\a ->
                 if a > 3
                   then 1
                   else 0)
              a
          !convoluted = mapStencil2 (BoundConst 0) st bin
          !stabilized = a +^ convoluted -^ (map (* 4) bin)
      computed <- computeP stabilized
      let unstable = V.any (> 3) $ toUnboxed computed
      D.trace ("#Topple: " B.++ (show count)) return ()
      if unstable
        then topple (count + 1) computed
        else return computed
    toppleRecord :: Int -> Array U DIM2 Int -> IO ([Array U DIM2 Int])
    toppleRecord count a = do
      let !st =
            [stencil2|  0 1 0
                          1 0 1
                          0 1 0 |]
          !bin =
            map
              (\a ->
                 if a > 3
                   then 1
                   else 0)
              a
          !convoluted = mapStencil2 (BoundConst 0) st bin
          !stabilized = a +^ convoluted -^ (map (* 4) bin)
      computed <- computeP stabilized
      let unstable = V.any (> 3) $ toUnboxed computed
      D.trace ("#Topple: " B.++ (show count)) return ()
      if unstable
        then (computed :) <$> (toppleRecord (count + 1) computed)
        else return [computed]
