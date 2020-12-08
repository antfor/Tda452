import Data.List

--0-----------------------------

drop' :: Int -> [a] -> [a]
drop' n xs | n <= 0 = xs
drop' _ [] = []
drop' n xs = drop' (n-1) (tail xs)

splitAt' n l = (take n l, drop n l)

zip3' :: [a] -> [b] -> [c] -> [(a,b,c)]
zip3' (x:a) (y:b) (z:c) = (x, y, z) : zip3' a b c
zip3' _ _ _ = []

zip3'' :: [a] -> [b] -> [c] -> [(a,b,c)]
zip3'' xs ys zs = [(x,y,z)|((x,y),z) <-zip (zip xs ys) zs]

--1-----------------------------
--2-----------------------------
--3-----------------------------
--4-----------------------------
--5-----------------------------
--6-----------------------------

--occursIn :: a -> [a] -> Bool
--occursIn = elem
--occursIn a as = not $ null [ x | x <- as , x == a ]

--allOccurIn :: [a] -> [a] -> Bool
--allOccurIn xs ys =  [x | x <- xs, y <- ys , x==y ]

--7-----------------------------
--8-----------------------------

pairs :: [a] -> [b] -> [(a,b)]
pairs xs ys = [(x,y) | x<-xs, y<-ys]

trip :: [(Int,Int,Int)]
trip = [(a,b,c) | a <-[1..100], b <- [a..100], c <- [b..100] , a^2 + b^2 == c^2]
