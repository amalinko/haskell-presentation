module EvalDemo (run) where

run :: IO ()
run = print $ show (factorial 5)

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)