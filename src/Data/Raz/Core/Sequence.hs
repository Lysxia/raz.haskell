module Data.Raz.Core.Sequence where

import Control.Monad.Random
import Prelude hiding (lookup)

import Data.Raz.Core

cons :: MonadRandom m => a -> Tree a -> m (Tree a)
cons a t = randomLevel <&> \lv ->
  joinSides (Bin lv 1 (Leaf a) Empty) t

snoc :: MonadRandom m => Tree a -> a -> m (Tree a)
snoc t a = randomLevel <&> \lv ->
  joinSides t (Bin lv 1 Empty (Leaf a))

append :: MonadRandom m => Tree a -> Tree a -> m (Tree a)
append t1 t2 = randomLevel <&> \lv ->
  t1 `joinSides` Bin lv 0 Empty Empty `joinSides` t2

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
