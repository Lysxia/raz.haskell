module Data.Raz.Core.Sequence where

import Control.Applicative
import Control.Monad ((>=>), join)
import Control.Monad.Random hiding (fromList)
import qualified Data.List as List
import Data.Maybe (listToMaybe)
import Prelude hiding
  (filter, lookup, take, drop, splitAt, zipWith)

import Data.Raz.Core
import Data.Raz.Util

-- * Construction

cons :: MonadRandom m => a -> Tree a -> m (Tree a)
cons a Empty = return (Leaf a)
cons a t = randomLevel <&> \lv ->
  joinSides (Bin lv 1 (Leaf a) Empty) t

snoc :: MonadRandom m => Tree a -> a -> m (Tree a)
snoc Empty a = return (Leaf a)
snoc t a = randomLevel <&> \lv ->
  joinSides t (Bin lv 1 Empty (Leaf a))

append :: MonadRandom m => Tree a -> Tree a -> m (Tree a)
append t1 Empty = return t1
append Empty t2 = return t2
append t1 t2 = randomLevel <&> \lv ->
  t1 `joinSides` Bin lv 0 Empty Empty `joinSides` t2

-- * Repetition

replicate :: MonadRandom m => Int -> a -> m (Tree a)
replicate n = fromList . List.replicate n

-- * Sublists

-- | /O(n*log n)/. The sequence of suffixes of a sequence.
tails :: MonadRandom m => Tree a -> m (Tree (Tree a))
tails = tailsWith return

-- | A generalization of 'tails' where the suffixes are passed to a
-- monadic continuation, recording their results.
tailsWith :: MonadRandom m => (Tree a -> m b) -> Tree a -> m (Tree b)
tailsWith f t = join $ liftA2 append (tailsWith' f t) (Leaf <$> f Empty)

-- | /O(n*log n)/. The sequence of non-empty suffixes of a sequence.
--
-- The underlying tree structure is reused to represent the overall sequence,
-- as well as every suffix, which might exacerbate worst case situations.
tailsWith' :: Applicative m => (Tree a -> m b) -> Tree a -> m (Tree b)
tailsWith' f (Leaf a) = Leaf <$> f (Leaf a)
tailsWith' f (Bin lv c l r) =
  Bin lv c
    <$> tailsWith' (\l' -> f (bin lv l' r)) l
    <*> tailsWith' f r
tailsWith' f Empty = pure Empty

-- * Sequential searches

takeWhileL :: (a -> Bool) -> Tree a -> Tree a
takeWhileL p = fst . spanL p

takeWhileR :: (a -> Bool) -> Tree a -> Tree a
takeWhileR p = snd . spanR p

dropWhileL :: (a -> Bool) -> Tree a -> Tree a
dropWhileL p = snd . spanL p

dropWhileR :: (a -> Bool) -> Tree a -> Tree a
dropWhileR p = fst . spanR p

spanL :: (a -> Bool) -> Tree a -> (Tree a, Tree a)
spanL p Empty = (Empty, Empty)
spanL p (Leaf a)
  | p a = (Leaf a, Empty)
  | otherwise = (Empty, Leaf a)
spanL p (Bin lv _ l r) =
  case spanL p l of
    (_, Empty) ->
      case spanL p r of
        (Empty, _) -> (l, r)
        (lr, rr) -> (bin lv l lr, rr)
    (ll, rl) -> (ll, bin lv rl r)

spanR :: (a -> Bool) -> Tree a -> (Tree a, Tree a)
spanR p Empty = (Empty, Empty)
spanR p (Leaf a)
  | p a = (Empty, Leaf a)
  | otherwise = (Leaf a, Empty)
spanR p (Bin lv _ l r) =
  case spanR p r of
    (Empty, _) ->
      case spanR p l of
        (_, Empty) -> (l, r)
        (ll, rl) -> (ll, bin lv rl r)
    (lr, rr) -> (bin lv l lr, rr)

breakL :: (a -> Bool) -> Tree a -> (Tree a, Tree a)
breakL p = spanL (not . p)

breakR :: (a -> Bool) -> Tree a -> (Tree a, Tree a)
breakR p = spanR (not . p)

partition :: (a -> Bool) -> Tree a -> (Tree a, Tree a)
partition p Empty = (Empty, Empty)
partition p t = (fold true, fold false)
  where
    (true, false) = partition' p t

partition' :: (a -> Bool) -> Tree a -> (TList a, TList a)
partition' p (Bin lv _ l r) = partition'' p lv r (partition' p l)
partition' p (Leaf a)
  | p a = (Tree (Leaf a) Nil, Nil)
  | otherwise = (Nil, Tree (Leaf a) Nil)

partition''
  :: (a -> Bool)
  -> Lev
  -> Tree a
  -> (TList a, TList a)
  -> (TList a, TList a)
partition'' p lv0 (Bin lv _ l r) tf =
  partition'' p lv r . partition'' p lv0 l $ tf
partition'' p lv0 (Leaf a) (true, false)
  | p a = (push' true, false)
  | otherwise = (true, push' false)
  where
    push' = Tree (Leaf a) . push lv0

filter p Empty = Empty
filter p (Leaf a)
  | p a = Leaf a
  | otherwise = Empty
filter p (Bin lv _ l r) =
  case (filter p l, filter p r) of
    (Empty, r') -> r'
    (l', Empty) -> l'
    (l', r') -> bin lv l' r'

-- * Indexing

-- | /log(n)/. The element at the specified position, counting from 0.
lookup :: Int -> Tree a -> Maybe a
lookup n = checked n (\t -> Just (viewC (focus n t))) Nothing

(!?) :: Tree a -> Int -> Maybe a
(!?) = flip lookup

index :: Tree a -> Int -> a
index t n = viewC (focus n t)

adjust :: (a -> a) -> Int -> Tree a -> Tree a
adjust f n t = checked n (unfocus . adjustC f . focus n) t t

adjust' :: (a -> a) -> Int -> Tree a -> Tree a
adjust' f n t = checked n (unfocus . adjustC' f . focus n) t t

update :: Int -> a -> Tree a -> Tree a
update n a t = checked n (unfocus . alterC a . focus n) t t

-- | Helper function for checking bounds.
checked :: Int -> (Tree a -> b) -> b -> Tree a -> b
checked n f b t
  | n < 0 || size t <= n = b
  | otherwise = f t

take :: Int -> Tree a -> Tree a
take n _ | n <= 0 = Empty
take _ (Leaf a) = Leaf a
take n t@(Bin lv c l r)
  | n <= size l = take n l
  | n < c = Bin lv n l (take (n - size l) r)
  | otherwise = t

drop :: Int -> Tree a -> Tree a
drop n t | n <= 0 = t
drop _ (Leaf _) = Empty
drop n (Bin lv c l r)
  | n < size l = Bin lv (c - n) (drop n l) r
  | n < c = drop (n - size l) r
  | otherwise = Empty

insertAt :: MonadRandom m => Int -> a -> Tree a -> m (Tree a)
insertAt = insertAt' L

deleteAt :: Int -> Tree a -> Tree a
deleteAt i t | i < 0 = t
deleteAt i Empty = Empty
deleteAt i (Leaf a) = Empty
deleteAt i t@(Bin lv c l r)
  | i < size l = Bin lv (c - 1) (deleteAt i l) r
  | i < c = Bin lv c l (deleteAt (i - size l) r)
  | otherwise = t

splitAt :: Int -> Tree a -> (Tree a, Tree a)
splitAt n t | n <= 0 = (Empty, t)
splitAt n Empty = (Empty, Empty)
splitAt n (Leaf a) = (Leaf a, Empty)
splitAt n t@(Bin lv c l r)
  | n < size l = let (ll, rl) = splitAt n l in (ll, Bin lv (c - n) rl r)
  | n == size l = (l, r)
  | n < c = let (lr, rr) = splitAt (n - size l) r in (Bin lv n l lr, rr)
  | otherwise = (t, Empty)

-- ** Indexing with predicates

elemIndexL :: Eq a => a -> Tree a -> Maybe Int
elemIndexL a = findIndexL (a ==)

elemIndicesL :: Eq a => a -> Tree a -> [Int]
elemIndicesL a = findIndicesL (a ==)

elemIndexR :: Eq a => a -> Tree a -> Maybe Int
elemIndexR a = findIndexR (a ==)

elemIndicesR :: Eq a => a -> Tree a -> [Int]
elemIndicesR a = findIndicesR (a ==)

findIndexL :: (a -> Bool) -> Tree a -> Maybe Int
findIndexL p = listToMaybe . findIndicesL p

findIndicesL :: (a -> Bool) -> Tree a -> [Int]
findIndicesL p t = findIndicesL' 0 p t []

findIndicesL' :: Int -> (a -> Bool) -> Tree a -> [Int] -> [Int]
findIndicesL' n p (Leaf a) | p a = (n :)
findIndicesL' n p (Bin _ _ l r)
  = findIndicesL' n p l . findIndicesL' (n + size l) p r
findIndicesL' _ _ _ = id

findIndexR :: (a -> Bool) -> Tree a -> Maybe Int
findIndexR p = listToMaybe . findIndicesR p

findIndicesR :: (a -> Bool) -> Tree a -> [Int]
findIndicesR p t = findIndicesR' 0 p t []

findIndicesR' :: Int -> (a -> Bool) -> Tree a -> [Int] -> [Int]
findIndicesR' n p (Leaf a) | p a = (n :)
findIndicesR' n p (Bin _ _ l r)
  = findIndicesR' (n + size l) p r . findIndicesR' n p l
findIndicesR' _ _ _ = id

-- * Transformations

mapWithIndex :: (Int -> a -> b) -> Tree a -> Tree b
mapWithIndex f t = mapWithIndex' f 0 t

mapWithIndex' :: (Int -> a -> b) -> Int -> Tree a -> Tree b
mapWithIndex' _ _ Empty = Empty
mapWithIndex' f n (Leaf a) = Leaf (f n a)
mapWithIndex' f n (Bin lv c l r) =
  Bin lv c (mapWithIndex' f n l) (mapWithIndex' f (n + size l) r)

traverseWithIndex :: Applicative f => (Int -> a -> f b) -> Tree a -> f (Tree b)
traverseWithIndex f t = traverseWithIndex' f 0 t

traverseWithIndex'
  :: Applicative f => (Int -> a -> f b) -> Int -> Tree a -> f (Tree b)
traverseWithIndex' _ _ Empty = pure Empty
traverseWithIndex' f n (Leaf a) = Leaf <$> f n a
traverseWithIndex' f n (Bin lv c l r) =
  Bin lv c <$> traverseWithIndex' f n l <*> traverseWithIndex' f (n + size l) r

-- * Zips

zip :: Tree a -> Tree b -> Tree (a, b)
zip = zipWith (,)

zipWith :: (a -> b -> c) -> Tree a -> Tree b -> Tree c
zipWith f a b = fold $ zipWith' f (Tree a Nil) (Tree b Nil) Nil

zipWith' :: (a -> b -> c) -> TList a -> TList b -> TList c -> TList c
zipWith' f Nil _ = id
zipWith' f _ Nil = id
zipWith' f as bs =
  case (trim R as, trim R bs) of
    (Cons a (Level la as'), Cons b (Level lb bs')) ->
      Tree (Leaf (f a b)) . push la

-- * Helpers

-- | A hacky implementation of the 'Applicative' product.

ap :: Tree (a -> b) -> Tree a -> Tree b
ap f (Leaf x) = ($ x) <$> f
ap _ Empty = Empty
ap f x@(Bin lv' c' _ _) = go f
  where
    go (Bin lv c l r) = Bin (lv + lv') (c * c') (go l) (go r)
    go (Leaf f) = f <$> x
    go Empty = Empty
