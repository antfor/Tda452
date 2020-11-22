import Test.QuickCheck

{- Lab 1
   Date: 04/11/2020
   Authors:Anton Forsberg
   Lab group: 31
 -}
--------------------------------------------
power :: Integer -> Integer -> Integer
power n k
   | k < 0 = error "power: negative argument"
power n 0  = 1
power n k  = n * power n (k-1)

-- A ------------------------
-- stepsPower n k gives the number of steps that
-- power n k takes to compute

stepsPower :: Integer -> Integer -> Integer
stepsPower n k = k + 1

-- B -------------------------
-- power1

power1 :: Integer -> Integer ->Integer
power1 n k
   | k < 0 = error "power: negative argument"
power1 n k = product (replicate (fromInteger k) n)


-- C -------------------------
-- power2

power2 :: Integer -> Integer ->Integer
power2 n k
   | k < 0 = error "power: negative argument"
power2 n k | k==0 = 1
power2 n k | even k = power2 (n*n) (div k 2)
power2 n k | odd k = n * power2 n (k-1)

-- D -------------------------
{-
--1
<Describe your test cases here>

  0^x is a special case we want to test because it always should be one.
  0,0 edge case for both n and k (n and k have the smalles possible value)
  0,1 test to se if it is correct for k=1 (k=1 is importent )
  0,5 test to see if it is good for a random number [k=5 in this case]

  1^x is a special case we want to test because it always should be one.
  Fore the same reason we want also test x^0
  1,7 see if 1^k works correctly for a random k (7 in this case).
  1,0 edge case for k (to se if it is correct for n=1)
  8,0 edge case for k (see if it is good for a random number [n=8 in this case] )
  0,0 edge case for both n and k (n and k have the smalles possible value)

  Testing with arbitrary numbers to hopefully catch potential problems me missed.
  24,7 test for a random k and n (to see if it generlly works).
  54,8 test for a random k and n  (to see if it generlly works).
  3,3 test where n and k are the same, for a random value
  23,23 test where n and k are the same
 -}

--2
prop_powers:: Integer -> Integer ->Bool
prop_powers n k = and [power n k == power1 n k, power n k == power2 n k] -- (power==power1 ,power == power2 => power1==power2)

--3
powerTest :: Bool
powerTest = and [prop_powers 0 0
                , prop_powers 0 1
                , prop_powers 0 5
                , prop_powers 1 0
                , prop_powers 8 0
                , prop_powers 1 7
                ,prop_powers 24 7
                ,prop_powers 54 8
                , prop_powers 3 3
                , prop_powers 23 23 ]

--4
prop_powers':: Integer -> Integer ->Bool
prop_powers' n k = prop_powers n (abs k)
