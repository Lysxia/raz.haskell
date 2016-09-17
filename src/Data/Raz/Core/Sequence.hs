module Data.Raz.Core.Sequence where

import Control.Monad.Random
import Prelude hiding (filter, lookup)

import Data.Raz.Core

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

-- * Sublists

-- | /O(n*log n)/. The sequence of suffixes of a sequence.
tails :: MonadRandom m => Tree a -> m (Tree (Tree a))
tails t = append (tails' t) (Leaf Empty)

-- | /O(n*log n)/. The sequence of non-empty suffixes of a sequence.
--
-- The underlying tree structure is reused to represent the overall sequence,
-- as well as every suffix, which might exacerbate worst case situations.
tails' :: Tree a -> Tree (Tree a)
tails' Empty = Empty
tails' t = tails'' t id

tails'' :: Tree a -> (Tree a -> Tree a) -> Tree (Tree a)
tails'' (Leaf a) k = Leaf (k (Leaf a))
tails'' (Bin lv c l r) k =
  Bin lv c
    (tails'' l $ \l' -> k (bin lv l' r))
    (tails'' r k)
tails'' Empty _ = error "internal error: empty tree"

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

breakl :: (a -> Bool) -> Tree a -> (Tree a, Tree a)
breakl p = spanL (not . p)

breakr :: (a -> Bool) -> Tree a -> (Tree a, Tree a)
breakr p = spanR (not . p)

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

checked :: Int -> (Tree a -> b) -> b -> Tree a -> b
checked n f b t
  | n < 0 || size t <= n = b
  | otherwise = f t
