{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
module Data.Raz.Sequence.Internal where

import Control.Applicative
import qualified Control.Monad as Monad
import Control.Monad.Random hiding (fromList)
import Data.Foldable (toList)
import qualified Data.List as List
import Data.Tuple (swap)
import System.IO.Unsafe

import qualified Data.Raz.Core as Raz
import qualified Data.Raz.Core.Sequence as Raz

data Seq' g a = Seq' !g !(Raz.Tree a)
  deriving (Functor, Foldable, Traversable)

instance Eq a => Eq (Seq' g a) where
  Seq' _ t == Seq' _ t' = t == t'

instance Ord a => Ord (Seq' g a) where
  compare (Seq' _ t) (Seq' _ t') = compare t t'

instance Show a => Show (Seq' g a) where
  showsPrec d (Seq' _ t) = showsPrec d t

type Seq = Seq' StdGen

-- * Construction

-- $implicit
-- Since RAZ makes use of randomness, a pure implementation will leak in the
-- interface (e.g., via 'MonadRandom' constraints or explicit generator
-- passing).
--
-- In order to provide the same interface as @Data.Sequence@ from @containers@,
-- we cheat by requesting a random generator via 'unsafePerformIO' and storing
-- it alongside the sequence when constructing it. This affects only
-- construction functions that have 'Seq' only in the output type.
--
-- Functions that transform existing sequences can then be implemented purely.
--
-- Alternative construction functions ('empty'', 'singleton'', 'fromList'')
-- are provided for compatibility with other random generators.

-- | /O(1)/. The empty sequence.
empty :: Seq a
empty = empty' (unsafePerformIO newStdGen)

-- | /O(1)/. A singleton sequence.
singleton :: a -> Seq a
singleton a = singleton' (unsafePerformIO newStdGen) a

fromList :: [a] -> Seq a
fromList as = fromList' (unsafePerformIO newStdGen) as

fromFunction :: Int -> (Int -> a) -> Seq a
fromFunction n f = fromList (fmap f [0 .. n - 1])

(<|) :: RandomGen g => a -> Seq' g a -> Seq' g a
a <| Seq' g Raz.Empty = Seq' g (Raz.Leaf a)
a <| as = seqBind as (Raz.cons a)

(|>) :: RandomGen g => Seq' g a -> a -> Seq' g a
Seq' g Raz.Empty |> a = Seq' g (Raz.Leaf a)
as |> a = seqBind as (`Raz.snoc` a)

(><) :: RandomGen g => Seq' g a -> Seq' g a -> Seq' g a
s1 >< Seq' _ t2 = seqBind s1 (\t1 -> Raz.append t1 t2)

-- | /O(1)/. The empty sequence.
empty' :: g -> Seq' g a
empty' g = Seq' g Raz.Empty

-- | /O(1)/. A singleton sequence.
singleton' :: g -> a -> Seq' g a
singleton' g = Seq' g . Raz.Leaf

fromList' :: RandomGen g => g -> [a] -> Seq' g a
fromList' g as = seqRun g (Raz.fromList as)

-- * Random generator manipulations

splitSeq :: RandomGen g => Seq' g a -> (Seq' g a, Seq' g a)
splitSeq (Seq' g t) = (Seq' g1 t, Seq' g2 t)
  where
    (g1, g2) = split g

seqBind :: Seq' g a -> (Raz.Tree a -> Rand g (Raz.Tree b)) -> Seq' g b
seqBind (Seq' g t) f = Seq' g' t'
  where
    (t', g') = runRand (f t) g

seqRun :: g -> Rand g (Raz.Tree a) -> Seq' g a
seqRun g t = Seq' g' t'
  where
    (t', g') = runRand t g

-- ** Repetition

-- | /O(n)/.
replicate :: Int -> a -> Seq a
replicate n a = fromList $ List.replicate n a

-- | /O(n)/.
replicateA :: Applicative f => Int -> f a -> f (Seq a)
replicateA n a = fromList <$> Monad.replicateM n a

-- | /O(n)/.
replicateM :: Monad m => Int -> m a -> m (Seq a)
replicateM n = unwrapMonad . replicateA n . WrapMonad

-- | /O(k)/.
cycleTaking :: RandomGen g => Int -> Seq' g a -> Seq' g a
cycleTaking k (Seq' g t) = fromList' g . take k . cycle . toList $ t

-- ** Iterative construction

iterateN :: Int -> (a -> a) -> a -> Seq a
iterateN n f = fromList . List.take n . List.iterate f

unfoldr :: (b -> Maybe (a, b)) -> b -> Seq a
unfoldr f = fromList . List.unfoldr f

unfoldl :: (b -> Maybe (b, a)) -> b -> Seq a
unfoldl f = fromList . reverse . List.unfoldr (fmap swap . f)

-- * Deconstruction

null :: Seq' g a -> Bool
null (Seq' _ Raz.Empty) = True
null _ = False

length :: Seq' g a -> Int
length (Seq' _ t) = Raz.size t

-- ** Views

data ViewL' g a
  = EmptyL
  | a :< Seq' g a

viewL :: Seq' g a -> ViewL' g a
viewL (Seq' _ Raz.Empty) = EmptyL
viewL (Seq' g t) = Raz.viewC raz :< Seq' g t'
  where
    raz = Raz.focusL $ t
    t' = Raz.unfocus . Raz.removeC Raz.R $ raz

