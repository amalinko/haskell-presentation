{-# ANN module ("HLint: ignore Eta reduce" :: String) #-}

module EvalDemo
  ( run
  ) where

import Control.Parallel.Strategies

run :: IO ()
run = do
  let ints = [1 .. 50000]
      calculated = filter (> 50) (calculateParallel ints)
  print $ length calculated

calculateSequentially :: [Int] -> [Int]
calculateSequentially ints = fmap numberOfDivisors ints

calculateParallel :: [Int] -> [Int]
calculateParallel ints = runEval $ myParMap numberOfDivisors ints

myParMap :: (a -> b) -> [a] -> Eval [b]
myParMap f [] = return []
myParMap f (x:xs) = do
  x' <- rpar (f x)
  xs' <- myParMap f xs
  return (x' : xs')

numberOfDivisors :: Int -> Int
numberOfDivisors n = length $ divisors n

divisors :: Int -> [Int]
divisors n = 1 : filter isDivisor [2 .. n `div` 2]
  where
    isDivisor x = rem n x == 0
