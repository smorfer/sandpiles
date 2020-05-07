{-# LANGUAGE BangPatterns #-}

module Control.Stabilization
  ( stabilize
  , stabilizeWithStats
  , stabilizeWithSteps
  ) where

import qualified Data.Vector.Unboxed          as V

import           Data.Array.Repa              ((+^), (-^))
import qualified Data.Array.Repa              as R
import           Data.Array.Repa.Stencil      as RS
import           Data.Array.Repa.Stencil.Dim2 as RS2D

import           Data.Sandpile
import           Data.Sandpile.Stencil

topple :: Monad m => Sandpile StaticPile -> m (Sandpile StaticPile)
topple (Pile pile size) = do
  let !binaryMap =
        R.map
          (\a ->
             if a > 3
               then 1
               else 0)
          pile
      !convoluted = mapStencil2 (BoundConst 0) toppleStencil binaryMap
      !stabilized = pile +^ convoluted -^ (R.map (* 4) binaryMap)
  ((flip Pile) size) <$> R.computeP stabilized

stabilize :: Monad m => Sandpile StaticPile -> m (Sandpile StaticPile)
stabilize sandpile@(Pile pile _) =
  if V.any (> 3) $ R.toUnboxed pile
    then stabilize =<< (topple sandpile)
    else return sandpile

stabilizeWithSteps :: Sandpile StaticPile -> IO (Sandpile StaticPile)
stabilizeWithSteps = stabilizeWithSteps' 0
  where
    stabilizeWithSteps' ::
         Int -> Sandpile StaticPile -> IO (Sandpile StaticPile)
    stabilizeWithSteps' count sandpile@(Pile pile _) =
      if V.any (> 3) $ R.toUnboxed pile
        then do
          putStrLn $ "#Topples: " ++ show count
          (stabilizeWithSteps' (succ count)) =<< (topple sandpile)
        else return sandpile

stabilizeWithStats :: Sandpile StaticPile -> IO (Sandpile StaticPile)
stabilizeWithStats = stabilizeWithStats' 0
  where
    stabilizeWithStats' ::
         Int -> Sandpile StaticPile -> IO (Sandpile StaticPile)
    stabilizeWithStats' count sandpile@(Pile pile _) =
      let !unstables =
            V.foldr'
              (\val count ->
                 if (val > 3)
                   then count + 1
                   else count)
              (0)
              (R.toUnboxed pile)
       in if unstables /= 0
            then do
              putStrLn $ "#Unstables: " ++ show unstables
              putStrLn $ "#Topples: " ++ show count
              (stabilizeWithStats' (succ count)) =<< (topple sandpile)
            else return sandpile
