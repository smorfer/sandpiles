{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes       #-}

module Data.Sandpile.Stencil
  ( toppleStencil
  ) where

import           Prelude                      (Int, Maybe (..))

import           Data.Array.Repa
import           Data.Array.Repa.Stencil
import           Data.Array.Repa.Stencil.Dim2

toppleStencil :: Stencil DIM2 Int
toppleStencil =
  [stencil2|  0 1 0
              1 0 1
              0 1 0 |]
