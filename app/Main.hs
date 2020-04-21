{-# LANGUAGE LambdaCase #-}


module Main where

import           Lib

import           Data.Matrix

import           Codec.Picture
import qualified Data.Vector                   as V
import           Debug.Trace                   as D
import           Control.DeepSeq

pileToColor :: Int -> PixelRGB8
pileToColor = \case
  0 -> PixelRGB8 0 0 0
  1 -> PixelRGB8 255 0 0
  2 -> PixelRGB8 0 255 255
  3 -> PixelRGB8 0 0 255
  n -> PixelRGB8 255 255 255

isUnstable :: Matrix Int -> Bool
isUnstable = foldr (\n b -> n > 3 || b) False

getUnstables :: Matrix Int -> [(Int, Int)]
getUnstables m = getter (nrows m) (ncols m) m
 where
  getter :: Int -> Int -> Matrix Int -> [(Int, Int)]
  getter 1 1 m = if gT3 1 1 m then [(1, 1)] else []
  getter r 1 m = if gT3 r 1 m
    then (r, 1) : getter (r - 1) (ncols m) m
    else getter (r - 1) (ncols m) m
  getter r c m =
    if gT3 r c m then (r, c) : getter r (c - 1) m else getter r (c - 1) m

  gT3 :: Int -> Int -> Matrix Int -> Bool
  gT3 r c m = getElem r c m > 3

toppleUnstables :: [(Int, Int)] -> Matrix Int -> Matrix Int
toppleUnstables [] m = m
toppleUnstables ((r, c) : us) m
  | r == 1 && c == 1
  = toppleUnstables us $ updateSelf r c $ updateBottom r c $ updateRight r c m
  | r == 1 && c == (ncols m)
  = toppleUnstables us $ updateSelf r c $ updateBottom r c $ updateLeft r c m
  | r == (nrows m) && c == 1
  = toppleUnstables us $ updateSelf r c $ updateTop r c $ updateRight r c m
  | r == (nrows m) && c == (ncols m)
  = toppleUnstables us $ updateSelf r c $ updateLeft r c $ updateTop r c m
  | r == 1
  = toppleUnstables us
    $ updateSelf r c
    $ updateLeft r c
    $ updateBottom r c
    $ updateRight r c m
  | c == 1
  = toppleUnstables us
    $ updateSelf r c
    $ updateRight r c
    $ updateTop r c
    $ updateBottom r c m
  | r == (nrows m)
  = toppleUnstables us
    $ updateSelf r c
    $ updateTop r c
    $ updateLeft r c
    $ updateRight r c m
  | c == (ncols m)
  = toppleUnstables us
    $ updateSelf r c
    $ updateLeft r c
    $ updateTop r c
    $ updateBottom r c m
  | otherwise
  = toppleUnstables us
    $ updateSelf r c
    $ updateLeft r c
    $ updateTop r c
    $ updateRight r c
    $ updateBottom r c m
 where
  updateElem change r c m = setElem ((getElem r c m) + change) (r, c) m
  updateSelf r c = updateElem (-4) r c
  updateLeft r c = updateElem 1 r (c - 1)
  updateTop r c = updateElem 1 (r - 1) c
  updateRight r c = updateElem 1 r (c + 1)
  updateBottom r c = updateElem 1 (r + 1) c

reduce :: Int -> Matrix Int -> Matrix Int
reduce count m = if isUnstable m
  then
    let nunstable = getUnstables m
        newM      = toppleUnstables nunstable m
    in  D.trace ("#T: " ++ show count ++ " #U: " ++ (show $ length nunstable))
                newM
        `deepseq` reduce (count + 1)
        $         newM
  else m

recordReduce :: Int -> Matrix Int -> [Matrix Int]
recordReduce count m = if isUnstable m
  then
    let nunstable = getUnstables m
        newM      = toppleUnstables (getUnstables m) m
    in  D.trace ("#T: " ++ show count ++ " #U: " ++ (show $ length nunstable))
                newM
        `deepseq` newM
        :         recordReduce (count + 1) newM
  else [m]


-- I = R(S - R(S))
main :: IO ()
main = do
  let size   = 32
      m3     = (matrix size size (\(x, y) -> 3))
      m      = (matrix size size (\(x, y) -> 6))
      record = False
  if not record
    then do
      putStrLn "Starting Toppling Simulation"
      let reduced = (reduce 1 $ m - (reduce 1 m))
      putStrLn "Simulated Toppling"
      let image = (\m -> generateImage (generator m) size size) reduced
      writeBitmap ("toppled" ++ show size ++ ".bmp") image
      putStrLn "Wrote Bitmap to disk"
    else do
      putStrLn "Starting Toppling Recording"
      let reducedRecorded = m : recordReduce 1 m
          images =
            (\m -> generateImage (generator m) size size) <$> reducedRecorded
          write = writeGifAnimation ("./toppling-" ++ show size ++ ".gif")
                                    1
                                    LoopingForever
                                    images
      case write of
        Left  str    -> putStrLn $ "Failed: " ++ str
        Right action -> putStrLn "Saved Gif" >> action
 where
  generator :: Matrix Int -> Int -> Int -> PixelRGB8
  generator m x y = pileToColor $ m ! (x + 1, y + 1)
