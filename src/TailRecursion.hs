module TailRecursion where

import Prelude hiding (lookup)

-- | 1. a) Tail-recursive lookup in an associative array.

assoc :: Int -> String -> [(String, Int)] -> Int
assoc def key kvs = error "TBD:assoc"

-- | 1. b) Return a copy of the given list with duplicate elements removed.

removeDuplicates :: [Int] -> [Int]
removeDuplicates l = reverse (helper [] l)
  where
    -- helper must be tail recursive
    helper :: [Int] -> [Int] -> [Int]
    helper seen []     = seen
    helper seen (x:xs) = helper seen' rest'
      where
        seen'          = error "TBD:helper:seen"
        rest'          = error "TBD:helper:rest"

-- | 1. c) Tail-recursive "loop".

wwhile :: (a -> (Bool, a)) -> a -> a
wwhile f n = error "TBD:wwhile"

-- | 1. d) Return the values produced by iterating a function until a fixpoint.

fixpointL :: (Int -> Int) -> Int -> [Int]
fixpointL f x = error "TBD:fixpointL"

-- The functions below can be used to test fixpointL and fixpointW.

g :: Int -> Int
g x = truncate (1e6 * cos (1e-6 * fromIntegral x))

collatz :: Int -> Int
collatz 1     = 1
collatz n
  | even n    = n `div` 2
  | otherwise = 3 * n + 1

-- | 1. e) Return the fixpoint of calling f repeatedly on x.

fixpointW :: (Int -> Int) -> Int -> Int
fixpointW f x = wwhile wwf x
 where
   wwf x      = error "TBD:fixpoint:wwf"
