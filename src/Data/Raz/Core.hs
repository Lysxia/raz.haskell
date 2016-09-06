{-# language BangPatterns #-}
module Data.Raz.Core where

import Control.Monad.Random
import Data.Bits
import Data.Function
import Prelude hiding (tail)

type Lev = Int
type Cnt = Int
data Dir = L | R deriving (Eq, Ord, Show, Read)

data Tree a
  = Empty
  | Leaf a
  | Bin !Lev !Cnt !(Tree a) !(Tree a)

data TList a
  = Nil
  | Cons a !(TList a)
  | Level Lev !(TList a)
  | Tree !(Tree a) !(TList a)

type Raz a = (TList a, a, TList a)

randomLevel :: MonadRandom m => m Lev
randomLevel = fmap (\n -> countTrailingZeros (n :: Word)) getRandom

singleton :: a -> Raz a
singleton a = (Nil, a, Nil)

empty :: MonadRandom m => a -> m (Raz a)
empty a = randomLevel <&> \lv ->
  (Level lv (Cons a Nil), a, Nil)

size :: Tree a -> Cnt
size Empty = 0
size (Leaf _) = 1
size (Bin _ c _ _) = c

trim :: Dir -> TList a -> TList a
trim d (Tree t rest) = trim' d t rest

trim' :: Dir -> Tree a -> TList a -> TList a
trim' _ (Leaf a) tl = Cons a tl
trim' d (Bin lv _ l r) tl =
  case d of
    L -> trim' d r (Level lv (Tree l tl))
    R -> trim' d l (Level lv (Tree r tl))

viewC :: Raz a -> a
viewC (_, a, _) = a

view :: Dir -> Raz a -> a
view L (l, _, _) = view' L l
view R (_, _, r) = view' R r

view' :: Dir -> TList a -> a
view' d Nil = error "view past end of seq"
view' d (Cons a _) = a
view' d (Level _ rest) = view' d rest
view' d t = view' d $ trim d t

alterC :: a -> Raz a -> Raz a
alterC a (l, _, r) = (l, a, r)

alter :: Dir -> a -> Raz a -> Raz a
alter L a (l, e, r) = (alter' L a l, e, r)
alter R a (l, e, r) = (l, e, alter' R a r)

alter' :: Dir -> a -> TList a -> TList a
alter' d a Nil = error "alter past end of seq"
alter' d a (Cons _ rest) = Cons a rest
alter' d a (Level lv rest) = Level lv $ alter' d a rest
alter' d a t = alter' d a (trim d t)

insert :: MonadRandom m => Dir -> a -> Raz a -> m (Raz a)
insert L a' (l, a, r) = randomLevel <&> \lv -> (Level lv (Cons a' l), a, r)
insert R a' (l, a, r) = randomLevel <&> \lv -> (l, a, Level lv (Cons a' r))

remove :: Dir -> Raz a -> Raz a
remove L (l, e, r) = (remove' L l, e, r)
remove R (l, e, r) = (l, e, remove' R r)

remove' :: Dir -> TList a -> TList a
remove' d Nil = error "remove past end of seq"
remove' d (Cons _ rest) = rest
remove' d (Level lv rest) = remove' d rest
remove' d t = remove' d (trim d t)

move :: Dir -> Raz a -> Raz a
move L (l, e, r) = move' L l (Cons e r)
move R (l, e, r) = move' R r (Cons e l) & \(r, e, l) -> (l, e, r)

move' :: Dir -> TList a -> TList a -> Raz a
move' d Nil s = error "move past end of seq"
move' d (Cons a rest) s = (rest, a, s)
move' d (Level lv rest) s = move' d rest (Level lv s)
move' d f s = move' d (trim d f) s

focus :: Int -> Tree a -> Raz a
focus p t | p < 0 || p >= size t = error "focus out of bounds"
focus p t = focus' p (Nil, Nil) t

focus' :: Int -> (TList a, TList a) -> Tree a -> Raz a
focus' _ _ Empty = error "internal Empty"
focus' p (l, r) (Leaf a) = (l, a, r) -- p == 0
focus' p (l, r) (Bin lv _ bl br)
  | p < c = focus' p (l, Level lv (Tree br r)) bl
  | otherwise = focus' (p - c) (Level lv (Tree bl l), r) br
  where c = size bl

joinSides :: Tree a -> Tree a -> Tree a
joinSides t1 t2 =
  case (t1, t2) of
    (Empty, _) -> t2
    (_, Empty) -> t1
    (Leaf _, Leaf _) -> error "leaf-leaf: full trees shouldn't be joined"
    (Leaf _, Bin lv _ l r) -> Bin lv tot (joinSides t1 l) r
    (Bin lv _ l r, Leaf _) -> Bin lv tot l (joinSides r t2)
    (Bin lv1 _ l1 r1, Bin lv2 _ l2 r2)
      | lv1 >= lv2 -> Bin lv1 tot l1 (joinSides r1 t2)
      | otherwise -> Bin lv2 tot (joinSides t1 l2) r2
  where tot = size t1 + size t2

headAsTree :: TList a -> Tree a
headAsTree Nil = Empty
headAsTree (Cons s _) = Leaf s
headAsTree (Level l _) = Bin l 0 Empty Empty
headAsTree (Tree t _) = t

tail :: TList a -> TList a
tail Nil = Nil -- ?
tail (Cons _ r) = r
tail (Level _ r) = r
tail (Tree _ r) = r

grow :: Dir -> TList a -> Tree a
grow d t = grow' d (headAsTree t) (tail t)

grow' :: Dir -> Tree a -> TList a -> Tree a
grow' _ h Nil = h
grow' d h t =
  case d of
    L -> grow' d (joinSides h' h) t'
    R -> grow' d (joinSides h h') t'
  where
    h' = headAsTree t
    t' = tail t

unfocus :: Raz a -> Tree a
unfocus (l, a, r) = joinSides (grow L l) . joinSides (Leaf a) $ grow R r

-- * Combinators

insertAt' :: MonadRandom m => Dir -> Int -> a -> Tree a -> m (Tree a)
insertAt' d i a = fmap unfocus . insert d a . focus i

-- * Displaying Raz

showRaz :: (a -> String) -> Raz a -> String
showRaz show (l', a, r') =
  let
    l = halfToList L l' []
    r = reverse (halfToList R r' [])
  in
    (foldl stringify `flip` r) .
    (\as -> as ++ ":" ++ show a ++ ":,") $
    foldl stringify "" l
  where
    stringify as a = as ++ show a ++ ", "

treeToList :: Dir -> Tree a -> [a] -> [a]
treeToList d Empty = id
treeToList d (Leaf a) = (a :)
treeToList L (Bin _ _ l r) = treeToList L l . treeToList L r
treeToList R (Bin _ _ l r) = treeToList R r . treeToList R l

halfToList :: Dir -> TList a -> [a] -> [a]
halfToList d Nil = id
halfToList d (Cons a rest) = halfToList d rest . (a :)
halfToList d (Level _ rest) = halfToList d rest
halfToList d (Tree t rest) = halfToList d rest . treeToList d t

-- * General functions

infixl 1 <&>
(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip fmap
