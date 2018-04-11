module EvalDemo
  ( run
  ) where

import System.Environment

run :: IO ()
run = do
  let ints = [2 .. 1000]
      calculated = fmap numberOfDivisors ints
  print $ show calculated

numberOfDivisors :: Int -> Int
numberOfDivisors n = length $ divisors n

divisors :: Int -> [Int]
divisors n = 1 : filter isDivisor [2 .. n `div` 2]
  where
    isDivisor x = rem n x == 0