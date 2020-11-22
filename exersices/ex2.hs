module Ex2 where
import Test.QuickCheck


-- E1 ------------------------

maxi:: Ord a => a -> a -> a
maxi x y | x > y = x
         | otherwise = y

prop_maxi::Integer -> Integer -> Bool
prop_maxi x y = m == x || m == y
    where
      m = maxi x y

-- E2-------------------------

sumsq :: Integral a => a -> a
sumsq 0 = 0
sumsq n = n * n + sumsq (n-1)

alternatve_sumsq n = sum [ i*i | i<-[1..n] ]


prop_sumsq n = n>0 ==> sumsq n == n*(n+1)*(2*n+1) `div` 6
-- E3 ------------------------


hanoi 0 = 0
hanoi n = hanoi (n-1) * 2 + 1

hanoi' 0 = 0
hanoi' 1 = 1
hanoi' n = 2 * hanoi' (n-2) +3


-- E4 ------------------------

fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibAux 0 a b = a
fibAux i a b = fibAux (i-1) b (a+b)

-- E5 ------------------------
smF :: Integer -> Integer
smF 1 = 1
smF n = smallestFactor n 2

smallestFactor :: Integer -> Integer -> Integer
smallestFactor n i | (n `mod` i) == 0 = i
                    | otherwise = smallestFactor n (i+1)

sumFac :: Integer -> Integer
sumFac 1 = 1
sumFac n = sumFac(n `div` (smF n)) + 1


allFactors n = [ f | f<-[1..n], n `mod` f == 0]

smallestF 1        = 1
smallestF n | n>1 = allFactors n !! 1

numFactors n = length (allFactors n)

-- E6 ------------------------

multiply :: Num a => [a] -> a
multiply [] = 1
multiply x:xs = x * multiply xs

--multiply :: Num a => [a] -> a
--multiply' xs =

-- E7 ------------------------

duplicates :: Eq a => [a] -> Bool
duplicates [] =

-- E8 ------------------------
-- E9 ------------------------
