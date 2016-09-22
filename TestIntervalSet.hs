-- http://hackage.haskell.org/package/QuickCheck
{- [1,2,3,5,8   --> [ (1,3), (5,5), (8,8)] bei anderen Werten z.B.  (5,4) --> [] direkt auf leere Menge abbilden untere Grenze kleiner als Große

Vereinigung? (1,3), (5,5), (8,8)] . [ (4,4)] --> [(1,5),(8,8)] oder [ (1,3),(5,5),(8,8)] . [(1,27)]

nicht zulässig:
[(1,2),(3,4)]

-}

module TestIntervalSet where

import           Data.IntervalSet
import           Test.QuickCheck

-- ----------------------------------------

-- invariant test: list of elems as input

prop_inv0 :: [Int] -> Bool
prop_inv0 = inv . fromList


-- invariant test: list of pairs

prop_inv :: [Pair] -> Bool
prop_inv = inv . fromPairs

-- all elements in set?
prop_elem :: [Pair] -> Bool
prop_elem ps = and (map isIn ps)
        where
                s = fromPairs ps
                isIn (P (x,y)) = all (`member` s) [x..y]


-- ----------------------------------------
--
-- auxiliary data type for Arbitrary instance
-- of Pairs

newtype Pair = P (Int, Int)
          deriving (Show)

instance Arbitrary Pair where
  arbitrary
    = do x <- arbitrary
         y <- arbitrary
         return (P (x `min` y, x `max` y))

fromPairs :: [Pair] -> IntervalSet
fromPairs = fromIntervalList . map (\ (P p) -> p)

-- ----------------------------------------

testArgs :: Args
testArgs
  = stdArgs { maxSize=500
            , maxSuccess=200
            }

t0, t1, t2 :: IO ()
t0 = quickCheck prop_inv0
t1 = quickCheck (verbose prop_inv)
t2 = quickCheckWith testArgs prop_elem

-- ----------------------------------------
