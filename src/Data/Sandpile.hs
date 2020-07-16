{-# LANGUAGE FlexibleInstances    #-}


module Data.Sandpile
  ( Sandpile(..)
  , StaticPile
  , DelayedPile
  , pileSize
  , toArray
  , zeroPile
  , filledPile
  ) where

import           Data.Array.Repa
import           Data.Array.Repa.Shape

class PileType t

instance PileType StaticPile

instance PileType DelayedPile

type Fill = Int

type Size = Int

type TemplatePile state = Array state DIM2 Int

type StaticPile = TemplatePile U

type DelayedPile = TemplatePile D

data Sandpile p = Pile p Size

instance Functor Sandpile where
  fmap f (Pile p s) = Pile (f p) s

pileSize :: Sandpile pileType -> Size
pileSize (Pile _ size) = size

toArray :: Sandpile piletype -> piletype
toArray (Pile piletype _) = piletype

zeroPile :: Size -> Sandpile StaticPile
zeroPile size =
  Pile
    (fromListUnboxed
       (Z :. (size :: Int) :. (size :: Int))
       (replicate (size * size) 0 :: [Int]))
    size

filledPile :: Fill -> Size -> Sandpile StaticPile
filledPile fill size =
  Pile
    (fromListUnboxed
       (Z :. (size :: Int) :. (size :: Int))
       (replicate (size * size) fill :: [Int]))
    size
