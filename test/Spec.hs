import Control.Applicative (liftA2)
import Control.Monad (void)
import Control.Monad.Random

import Data.Raz.Core

infixl 1 <&>
(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip fmap

printRaz :: Raz String -> IO (Raz String)
printRaz = liftA2 (>>) (putStrLn . showRaz id) return

printC :: Raz String -> IO (Raz String)
printC = liftA2 (>>) (putStrLn . viewC) return

x0 :: IO (Tree Int)
x0 = return . unfocus $ singleton 3
x1 = insertAt' L 0 1 =<< x0
x2 = insertAt' L 0 0 =<< x1
x3 = insertAt' R 1 2 =<< x2
x4 = insertAt' R 3 4 =<< x3
y4 = fmap (focus 2) x4 -- 0, 1, :2:, 3, 4

lotsOfRefocus :: Raz a -> IO (Raz a)
lotsOfRefocus = iterateM 100 $ \raz -> do
  getRandomR (0, 13) <&> \i -> focus i . unfocus $ raz

iterateM 0 _ x = return x
iterateM n f x = iterateM (n-1) f x >>= f

pop d = liftA2 (<$) (remove d) (putStrLn . view d)

main :: IO ()
main = do
  void $
    foldl (\raz x -> raz >>= insert L [x])
      (return (singleton "r"))
      "efghijklmnopq"
    <&> unfocus <&> focus 4 <&> move L >>= printC
    <&> unfocus <&> focus 7 >>= printC
    <&> alterC "L" <&> unfocus <&> focus 7 >>= printC >>= printRaz
    >>= lotsOfRefocus >>= printRaz
  void $
    foldl (\raz x -> raz >>= insert L [x]) (empty "") "12345"
    <&> unfocus <&> focus 0
    >>= pop R >>= pop R >>= pop R

