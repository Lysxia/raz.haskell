{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
module Data.Raz.Core where

import Control.Monad.Random hiding (fromList)
import Data.Bits
import Data.Foldable (toList)
import Data.Function
import Prelude hiding (tail)
import System.IO.Unsafe
import Text.Read

type Lev = Int
type Cnt = Int
data Dir = L | R deriving (Eq, Ord, Show, Read)

data Tree a
  = Empty
  | Leaf a
  | Bin !Lev !Cnt !(Tree a) !(Tree a)
  deriving (Functor, Foldable, Traversable)

data TList a
  = Nil
  | Cons a !(TList a)
  | Level Lev !(TList a)
  | Tree !(Tree a) !(TList a)
  deriving (Eq, Ord, Functor, Foldable, Traversable)

type Raz a = (TList a, a, TList a)

instance Eq a => Eq (Tree a) where
  (==) = (==) `on` toList

instance Ord a => Ord (Tree a) where
  compare = compare `on` toList

instance Show a => Show (Tree a) where
  showsPrec d t = showParen (d > 10) $
    showString "fromList " . shows (toList t)

instance Read a => Read (Tree a) where
  readPrec = parens . prec 10 $ do
    Ident "fromList" <- lexP
    m <- step readListPrec
    return (evalRand (fromList m) (unsafePerformIO newStdGen))

  readListPrec = readListPrecDefault

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

adjustC :: (a -> a) -> Raz a -> Raz a
adjustC f (l, a, r) = (l, f a, r)

adjustC' :: (a -> a) -> Raz a -> Raz a
adjustC' f (l, a, r) = let a' = f a in a' `seq` (l, a', r)

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
remove L (l, e, r) = (snd $ remove' L l, e, r)
remove R (l, e, r) = (l, e, snd $ remove' R r)

remove' :: Dir -> TList a -> (a, TList a)
remove' d Nil = error "remove past end of seq"
remove' d (Cons a rest) = (a, rest)
remove' d (Level lv rest) = remove' d rest
remove' d t = remove' d (trim d t)

removeC :: Dir -> Raz a -> Raz a
removeC L (l, _, r) = (l', a, r)
  where
    (a, l') = remove' L l
removeC R (l, _, r) = (l, a, r')
  where
    (a, r') = remove' R r

move :: Dir -> Raz a -> Raz a
move L (l, e, r) = move' L l (Cons e r)
move R (l, e, r) = move' R r (Cons e l)

move' :: Dir -> TList a -> TList a -> Raz a
move' d Nil s = error "move past end of seq"
move' L (Cons a rest) s = (rest, a, s)
move' R (Cons a rest) s = (s, a, rest)
move' d (Level lv rest) s = move' d rest (Level lv s)
move' d f s = move' d (trim d f) s

focus :: Int -> Tree a -> Raz a
focus p t | p < 0 || p >= size t = error "focus out of bounds"
focus p t = focus' p Nil Nil t

focus' :: Int -> TList a -> TList a -> Tree a -> Raz a
focus' _ _ _ Empty = error "internal Empty"
focus' p l r (Leaf a) = (l, a, r) -- p == 0
focus' p l r (Bin lv _ bl br)
  | p < c = focus' p l (Level lv (Tree br r)) bl
  | otherwise = focus' (p - c) (Level lv (Tree bl l)) r br
  where c = size bl

focusL :: Tree a -> Raz a
focusL = focusL' Nil

focusL' :: TList a -> Tree a -> Raz a
focusL' _ Empty = error "internal Empty"
focusL' r (Leaf a) = (Nil, a, r)
focusL' r (Bin lv _ bl br) = focusL' (Level lv (Tree br r)) bl

focusR :: Tree a -> Raz a
focusR = focusR' Nil

focusR' :: TList a -> Tree a -> Raz a
focusR' _ Empty = error "internal Empty"
focusR' l (Leaf a) = (l, a, Nil)
focusR' l (Bin lv _ bl br) = focusR' (Level lv (Tree bl l)) br

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

fromList :: MonadRandom m => [a] -> m (Tree a)
fromList [] = return Empty
fromList (a : as) =
  fromList' (Tree (Leaf a) Nil) as

fromList'
  :: MonadRandom m => TList a -> [a] -> m (Tree a)
fromList' !ls [] = return (fold ls)
fromList' !ls (a : as) =
  randomLevel >>= \lv ->
    fromList' (Tree (Leaf a) (push lv ls)) as

push :: Lev -> TList a -> TList a
push lv (Tree t (Level lv' (Tree t' ls)))
  | lv > lv' = push lv (Tree (bin lv' t' t) ls)
push lv ls = Level lv ls

fold :: TList a -> Tree a
fold (Tree t (Level lv (Tree t' ls))) = fold (Tree (bin lv t' t) ls)
fold (Tree t _) = t
fold _ = error "internal error"

bin :: Lev -> Tree a -> Tree a -> Tree a
bin lv l r = Bin lv tot l r
  where
    tot = size l + size r

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

showTree :: (a -> String) -> Tree a -> String
showTree show' Empty = "Empty"
showTree show' (Leaf a) = show' a
showTree show' (Bin lv _ l r) =
  "(" ++ showTree show' l ++ ") " ++
  show lv ++
  " (" ++ showTree show' r ++ ")"

-- * General functions

infixl 1 <&>
(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip fmap
