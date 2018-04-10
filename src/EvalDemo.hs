module EvalDemo (run) where

import System.Environment

run :: IO ()
run = do
   args <- getArgs
   let
      ints = [2..100]
      factorials = fmap factorial ints
   print $ show factorials

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)