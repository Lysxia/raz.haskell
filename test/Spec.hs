import Data.Raz.Core

x0 :: IO (Tree Int)
x0 = return . unfocus $ singleton 3
x1 = insertAt L 0 1 =<< x0
x2 = insertAt L 0 0 =<< x1
x3 = insertAt R 1 2 =<< x2
x4 = insertAt R 3 4 =<< x3
y4 = fmap (flip focus 2) x4 -- 0, 1, :2:, 3, 4

main :: IO ()
main = putStrLn . showRaz show =<< y4
