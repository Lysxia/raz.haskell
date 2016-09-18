-- | Miscellaneous instances for 'Tree''.

module Data.Raz.Core.Instances where

import Control.DeepSeq

import Data.Raz.Core.Internal

instance NFData a => NFData (Tree a) where
  rnf Empty = ()
  rnf (Leaf a) = rnf a
  rnf (Bin _ _ l r) = rnf l `seq` rnf r
