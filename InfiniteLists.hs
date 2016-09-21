module InfiniteLists where

-- | construct a name generator
--
-- names = ["a".."z", "a1".."z1", "a2".."z2", ...]

names :: [String]
names = [['a'..'z']] ++ [ c: (show i) |  i <- [1..] , c <- ['a'..'z'] ]

-- | constructs the infinite sequence
-- of fibonacci numbers in linear time
--
-- fibs = [0, 1, 1, 2, 3, 5, 8, ...]
-- 0 1 1 2 3 5 8 13
-- - 0 1 1 2 3 5 8
-- - - 0 1 1 2 3 5
-- fibs = ... : .. : zipwith (+) xs ys
-- when xs=
--      ys=

fibs :: [Integer]

fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
                          
-- ----------------------------------------
--
-- | naive prime number generator with
-- sieve of Eratosthenes and a recursive
-- sieve operation

primes :: [Integer]
primes = primes' [2..]
primes' (x:xs) = x : primes' [ y | y <- xs, y `mod` x /= 0 ]

-- ----------------------------------------
--
-- | the hamiltonian sequence is the ordered sequence
-- of natural number which are multiples of 2 or 3 or 5
--
-- Implementation: the 3 lists of multiples of 2, 3 and 5
-- are merged together with a @merges@ function.
--
-- The direct solution is

hamilton' :: [Integer]
hamilton'
  = filter
    (\ i ->    i `mod` 2 == 0
            || i `mod` 3 == 0
            || i `mod` 5 == 0
    ) [0..]

-- | @hamilton@ by merging sequences

hamilton :: [Integer]
hamilton
  = merges [is2, is3, is5]
    where
      is2 = [ x | x <- [0..], x `mod` 2 == 0]
      is3 = [ x | x <- [3..], x `mod` 3 == 0]
      is5 = [ x | x <- [5..], x `mod` 5 == 0]

merge :: [Integer] -> [Integer] -> [Integer]

merge xs     []     = xs
merge []     ys     = ys
merge (x:xs) (y:ys) 
                    | x == y = x : merge xs ys
                    | x > y = y : merge (x:xs) ys 
                    | y > x = x : merge xs (y:ys)


                    
-- | @merges@ takes a list of lists of ascending integers
-- and merges these lists into a single sorted list without any duplicates
-- direct impl

-- [ [1,2,3] , [4,5,6], [4,5,6] ] --> [1,2,3,4,5,6]

merges :: [[Integer]] -> [Integer]
merges []     = []
merges (x:xs) = merge x (merges xs)


-- | @merges@ with a fold

merges' :: [[Integer]] -> [Integer]
merges' = undefined    -- after chapter about folds

-- ----------------------------------------
