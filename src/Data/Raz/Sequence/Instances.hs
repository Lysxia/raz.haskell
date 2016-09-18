-- | Miscellaneous instances for 'Seq''.
--
-- They will be reexported by 'Data.Raz.Sequence'.

module Data.Raz.Sequence.Instances where

import Control.DeepSeq

import Data.Raz.Core.Instances
import Data.Raz.Sequence.Internal

-- | 'StdGen' is not an instance of 'NFData', and random generators
-- should be pretty strict for efficiency anyway, so we don't require
-- and 'NFData' for them.
instance NFData a => NFData (Seq' g a) where
  rnf (Seq' g t) = g `seq` rnf t `seq` ()
