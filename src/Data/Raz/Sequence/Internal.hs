{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.Raz.Sequence.Internal where

import Control.Applicative
import qualified Control.Monad as Monad
import Control.Monad.Random hiding (fromList)
import Data.Foldable (toList)
import qualified Data.List as List
import Data.Tuple (swap)
import System.IO.Unsafe
import Prelude hiding (lookup, zipWith)

import qualified Data.Raz.Core.Internal as Raz
import qualified Data.Raz.Core.Sequence as Raz
import Data.Raz.Util

-- * Types

data Seq' g a = Seq' !g !(Raz.Tree a)
  deriving (Functor, Foldable, Traversable)

instance Eq a => Eq (Seq' g a) where
  Seq' _ t == Seq' _ t' = t == t'

instance Ord a => Ord (Seq' g a) where
  compare (Seq' _ t) (Seq' _ t') = compare t t'

instance Show a => Show (Seq' g a) where
  showsPrec d (Seq' _ t) = showsPrec d t

-- |
-- @
-- pure :: a -> 'Impure' ('Seq' a)
-- (\<*\>) :: 'Seq' (a -> b) -> 'Seq' a -> 'Seq' b
-- @
instance Applicative (Seq' StdGen) where
  pure a = createSeq (Raz.Leaf a)
  (<*>) = seqLift2 Raz.ap

-- ** Synonyms

-- $convention
-- The actual type signatures of functions in this module are as general as
-- possible. Alternative signatures may appear in comments:
--
--   - a purely informative "enriched" type with 'Impure' and 'Splittable'
--   marking certain implementation details;
--
--   - a specialization at type 'Seq' that parallels the @Data.Sequence@ API.

-- | The sequence type with a default generator.
type Seq = Seq' StdGen

-- | A type synonym for documentation. Marks uses of 'unsafePerformIO'.
type Impure a = a

-- | A type synonym for documentation. Marks functions that only use the
-- 'split' method of random generators.
type Splittable g = RandomGen g

-- * Construction

-- $implicit
-- Since RAZ makes use of randomness, a pure implementation will leak in the
-- interface (e.g., via 'MonadRandom' constraints or explicit generator
-- passing).
--
-- In order to provide the same interface as @Data.Sequence@ from @containers@,
-- we cheat by requesting a random generator via 'unsafePerformIO' and storing
-- it alongside the sequence when constructing it.
--
-- Functions that transform existing sequences can then be implemented purely.
--
-- Alternative construction functions ('empty'', 'singleton'', 'fromList'')
-- are provided for compatibility with other random generators.

-- |
-- @
-- empty :: 'Impure' (Seq a)
-- @
--
-- /O(1)/. The empty sequence.
empty :: Seq a
empty = empty' (unsafePerformIO newStdGen)

-- |
-- @
-- singleton :: a -> 'Impure' (Seq a)
-- @
--
-- /O(1)/. A singleton sequence.
singleton :: a -> Seq a
singleton a = singleton' (unsafePerformIO newStdGen) a

-- |
-- @
-- fromList :: [a] -> 'Impure' (Seq a)
-- @
--
-- /O(n)/. Create a sequence from a finite list of elements.
--
-- The inverse 'toList' is given by the 'Foldable' instance of 'Seq'.
fromList :: [a] -> Seq a
fromList as = fromList' (unsafePerformIO newStdGen) as

-- |
-- @
-- fromFunction :: Int -> (Int -> a) -> 'Impure' (Seq a)
-- fromFunction n f = fromList (fmap f [0 .. n - 1])
-- @
--
-- /O(n)/.
fromFunction :: Int -> (Int -> a) -> Seq a
fromFunction n f = fromList (fmap f [0 .. n - 1])

-- |
-- @
-- (\<|) :: a -> 'Seq' a -> 'Seq' a
-- @
(<|) :: RandomGen g => a -> Seq' g a -> Seq' g a
a <| as = seqBind as (Raz.cons a)

-- |
-- @
-- (|>) :: 'Seq' a -> a -> 'Seq' a
-- @
(|>) :: RandomGen g => Seq' g a -> a -> Seq' g a
as |> a = seqBind as (`Raz.snoc` a)

-- |
-- @
-- (>\<) :: 'Seq' a -> 'Seq' a -> 'Seq' a
-- @
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

-- ** Repetition

-- |
-- @
-- replicate :: Int -> a -> 'Impure' (Seq a)
-- @
--
-- /O(n)/.
replicate :: Int -> a -> Seq a
replicate n a = fromList $ List.replicate n a

-- |
-- @
-- replicateA :: Applicative f => Int -> f a -> f ('Impure' (Seq a))
-- @
--
-- /O(n)/.
replicateA :: Applicative f => Int -> f a -> f (Seq a)
replicateA n a = fromList <$> Monad.replicateM n a

-- |
-- @
-- replicateM :: Monad m => Int -> m a -> m ('Impure' (Seq a))
-- @
--
-- /O(n)/.
replicateM :: Monad m => Int -> m a -> m (Seq a)
replicateM n = unwrapMonad . replicateA n . WrapMonad

-- |
-- @
-- cycleTaking :: Int -> 'Seq' a -> 'Seq' a
-- @
--
-- /O(k)/.
cycleTaking :: RandomGen g => Int -> Seq' g a -> Seq' g a
cycleTaking k (Seq' g t) = fromList' g . List.take k . cycle . toList $ t

-- ** Iterative construction

-- |
-- @
-- iterateN :: Int -> (a -> a) -> a -> 'Impure' (Seq a)
-- @
--
-- /O(n)/.
iterateN :: Int -> (a -> a) -> a -> Seq a
iterateN n f = fromList . List.take n . List.iterate f

-- |
-- @
-- unfoldr :: (b -> Maybe (a, b)) -> b -> 'Impure' (Seq a)
-- @
--
-- /O(n)/, where @n@ is the length of the output sequence.
unfoldr :: (b -> Maybe (a, b)) -> b -> Seq a
unfoldr f = fromList . List.unfoldr f

-- |
-- @
-- unfoldl :: (b -> Maybe (b, a)) -> b -> 'Impure' (Seq a)
-- @
--
-- /O(n)/, where @n@ is the length of the output sequence.
unfoldl :: (b -> Maybe (b, a)) -> b -> Seq a
unfoldl f = fromList . reverse . List.unfoldr (fmap swap . f)

-- * Deconstruction

-- ** Queries

-- |
-- @
-- null :: 'Seq' a -> Bool
-- @
--
-- /O(1)/. Is this the empty sequence?
null :: Seq' g a -> Bool
null (Seq' _ Raz.Empty) = True
null _ = False

-- |
-- @
-- length :: 'Seq' a -> Int
-- @
--
-- /O(1)/. The number of elements in the sequence.
length :: Seq' g a -> Int
length (Seq' _ t) = Raz.size t

-- ** Views

data ViewL' g a
  = EmptyL
  | a :< Seq' g a

viewl :: Seq' g a -> ViewL' g a
viewl (Seq' _ Raz.Empty) = EmptyL
viewl (Seq' g t) = Raz.viewC raz :< Seq' g t'
  where
    raz = Raz.focusL t
    t' = Raz.unfocus . Raz.removeC Raz.R $ raz

-- * Sublists

-- |
-- @
-- tails :: 'Splittable' g => Seq' g a -> Seq' g (Seq' g a)
-- tails :: 'Seq' a -> 'Seq' ('Seq' a)
-- @
tails :: RandomGen g => Seq' g a -> Seq' g (Seq' g a)
tails s = seqBind s (Raz.tailsWith splitting)
  where
    splitting a = getSplit <&> \g -> Seq' g a

-- ** Sequential searches

-- |
-- @
-- takeWhileL :: (a -> Bool) -> Seq a -> Seq a
-- @
takeWhileL :: (a -> Bool) -> Seq' g a -> Seq' g a
takeWhileL = seqLift . Raz.takeWhileL

-- |
-- @
-- takeWhileR :: (a -> Bool) -> 'Seq' a -> 'Seq' a
-- @
takeWhileR :: (a -> Bool) -> Seq' g a -> Seq' g a
takeWhileR = seqLift . Raz.takeWhileR

-- |
-- @
-- dropWhileL :: (a -> Bool) -> 'Seq' a -> 'Seq' a
-- @
dropWhileL :: (a -> Bool) -> Seq' g a -> Seq' g a
dropWhileL = seqLift . Raz.dropWhileL

-- |
-- @
-- dropWhileR :: (a -> Bool) -> 'Seq' a -> 'Seq' a
-- @
dropWhileR :: (a -> Bool) -> Seq' g a -> Seq' g a
dropWhileR = seqLift . Raz.dropWhileR

-- |
-- @
-- spanl :: 'Splittable' g => (a -> Bool) -> Seq' g a -> (Seq' g a, Seq' g a)
-- spanl :: (a -> Bool) -> Seq a -> (Seq a, Seq a)
-- @
spanl :: RandomGen g => (a -> Bool) -> Seq' g a -> (Seq' g a, Seq' g a)
spanl = seqLiftSplit . Raz.spanL

-- |
-- @
-- spanr :: 'Splittable' g => (a -> Bool) -> Seq' g a -> (Seq' g a, Seq' g a)
-- spanr :: (a -> Bool) -> 'Seq' a -> ('Seq' a, 'Seq' a)
-- @
spanr :: RandomGen g => (a -> Bool) -> Seq' g a -> (Seq' g a, Seq' g a)
spanr = seqLiftSplit . Raz.spanR

-- |
-- @
-- breakl :: 'Splittable' g => (a -> Bool) -> Seq' g a -> (Seq' g a, Seq' g a)
-- breakl :: (a -> Bool) -> 'Seq' a -> ('Seq' a, 'Seq' a)
-- @
breakl :: RandomGen g => (a -> Bool) -> Seq' g a -> (Seq' g a, Seq' g a)
breakl = seqLiftSplit . Raz.breakL

-- |
-- @
-- breakr :: 'Splittable' g => (a -> Bool) -> Seq' g a -> (Seq' g a, Seq' g a)
-- breakr :: (a -> Bool) -> 'Seq' a -> ('Seq' a, 'Seq' a)
-- @
breakr :: RandomGen g => (a -> Bool) -> Seq' g a -> (Seq' g a, Seq' g a)
breakr = seqLiftSplit . Raz.breakR

-- |
-- @
-- partition :: 'Splittable' g => (a -> Bool) -> Seq' g a -> (Seq' g a, Seq' g a)
-- partition :: (a -> Bool) -> 'Seq' a -> ('Seq' a, 'Seq' a)
-- @
partition :: RandomGen g => (a -> Bool) -> Seq' g a -> (Seq' g a, Seq' g a)
partition = seqLiftSplit . Raz.partition

-- |
-- @
-- filter :: (a -> Bool) -> 'Seq' a -> 'Seq' a
-- @
filter :: (a -> Bool) -> Seq' g a -> Seq' g a
filter = seqLift . Raz.filter

-- * Indexing

-- |
-- @
-- lookup :: Int -> 'Seq' a -> Maybe a
-- @
lookup :: Int -> Seq' g a -> Maybe a
lookup = seqApply . Raz.lookup

-- |
-- @
-- (?!) :: 'Seq' a -> Int -> Maybe a
-- @
(?!) :: Seq' g a -> Int -> Maybe a
(?!) = flip lookup

-- |
-- @
-- index :: 'Seq' a -> Int -> a
-- @
index :: Seq' g a -> Int -> a
index s n = seqApply (\t -> Raz.index t n) s

-- |
-- @
-- adjust :: (a -> a) -> Int -> 'Seq' a -> 'Seq' a
-- @
adjust :: (a -> a) -> Int -> Seq' g a -> Seq' g a
adjust f = seqLift . Raz.adjust f

-- b
-- |
-- @
-- adjust' :: (a -> a) -> Int -> 'Seq' a -> 'Seq' a
-- @
adjust' :: (a -> a) -> Int -> Seq' g a -> Seq' g a
adjust' f = seqLift . Raz.adjust' f

-- |
-- @
-- update :: Int -> a -> 'Seq' a -> 'Seq' a
-- @
update :: Int -> a -> Seq' g a -> Seq' g a
update n = seqLift . Raz.update n

-- |
-- @
-- take :: Int -> 'Seq' a -> 'Seq' a
-- @
take :: Int -> Seq' g a -> Seq' g a
take = seqLift . Raz.take

-- |
-- @
-- drop :: Int -> 'Seq' a -> 'Seq' a
-- @
drop :: Int -> Seq' g a -> Seq' g a
drop = seqLift . Raz.drop

-- |
-- @
-- insertAt :: Int -> a -> 'Seq' a -> 'Seq' a
-- @
insertAt :: RandomGen g => Int -> a -> Seq' g a -> Seq' g a
insertAt n = seqDnib . Raz.insertAt n

-- |
-- @
-- deleteAt :: Int -> 'Seq' a -> 'Seq' a
-- @
deleteAt :: Int -> Seq' g a -> Seq' g a
deleteAt = seqLift . Raz.deleteAt

-- |
-- @
-- splitAt :: 'Splittable' g => Int -> Seq' g a -> (Seq' g a, Seq' g a)
-- splitAt :: Int -> 'Seq' a -> ('Seq' a, 'Seq' a)
-- @
splitAt :: RandomGen g => Int -> Seq' g a -> (Seq' g a, Seq' g a)
splitAt = seqLiftSplit . Raz.splitAt

-- * Transformations

-- |
-- @
-- mapWithIndex :: (Int -> a -> b) -> 'Seq' a -> 'Seq' b
-- @
mapWithIndex :: (Int -> a -> b) -> Seq' g a -> Seq' g b
mapWithIndex = seqLift . Raz.mapWithIndex

-- |
-- @
-- traverseWithIndex :: Applicative f => (Int -> a -> f b) -> 'Seq' a -> 'Seq' b
-- @
traverseWithIndex
  :: Applicative f => (Int -> a -> f b) -> Seq' g a -> f (Seq' g b)
traverseWithIndex = seqLens . Raz.traverseWithIndex

-- * Zips

-- |
-- @
-- zip :: 'Seq' a -> 'Seq' b -> 'Seq' (a, b)
-- @
zip :: Seq' g a -> Seq' g b -> Seq' g (a, b)
zip = zipWith (,)

-- |
-- @
-- zipWith :: (a -> b -> c) -> 'Seq' a -> 'Seq' b -> 'Seq' c
-- @
zipWith :: (a -> b -> c) -> Seq' g a -> Seq' g b -> Seq' g c
zipWith = seqLift2 . Raz.zipWith

-- * Random generator manipulation

splitSeq :: Splittable g => Seq' g a -> (Seq' g a, Seq' g a)
splitSeq (Seq' g t) = (Seq' g1 t, Seq' g2 t)
  where
    (g1, g2) = split g

-- | Put a fresh generator.
refreshSeq :: Seq' g a -> Impure (Seq a)
refreshSeq (Seq' _ t) = createSeq t

-- | Wrap a 'Raz.Tree' into a 'Seq'.
createSeq :: Raz.Tree a -> Impure (Seq a)
createSeq t = Seq' (unsafePerformIO newStdGen) t

seqBind :: Seq' g a -> (Raz.Tree a -> Rand g (Raz.Tree b)) -> Seq' g b
seqBind (Seq' g t) f = Seq' g' t'
  where
    (t', g') = runRand (f t) g

seqDnib :: (Raz.Tree a -> Rand g (Raz.Tree b)) -> Seq' g a -> Seq' g b
seqDnib = flip seqBind

seqRun :: g -> Rand g (Raz.Tree a) -> Seq' g a
seqRun g t = Seq' g' t'
  where
    (t', g') = runRand t g

seqLift :: (Raz.Tree a -> Raz.Tree b) -> Seq' g a -> Seq' g b
seqLift f (Seq' g t) = Seq' g (f t)

seqLift2
  :: (Raz.Tree a -> Raz.Tree b -> Raz.Tree c)
  -> Seq' g a -> Seq' g b -> Seq' g c
seqLift2 f (Seq' g ta) (Seq' _ tb) = Seq' g (f ta tb)

seqLiftSplit
  :: Splittable g
  => (Raz.Tree a -> (Raz.Tree b, Raz.Tree c))
  -> Seq' g a -> (Seq' g b, Seq' g c)
seqLiftSplit f (Seq' g t) = (Seq' g1 t1, Seq' g2 t2)
  where
    (g1, g2) = split g
    (t1, t2) = f t

seqApply :: (Raz.Tree a -> b) -> Seq' g a -> b
seqApply f (Seq' _ t) = f t

seqLens
  :: Functor f
  => (Raz.Tree a -> f (Raz.Tree b))
  -> Seq' g a -> f (Seq' g b)
seqLens f (Seq' g a) = Seq' g <$> f a
