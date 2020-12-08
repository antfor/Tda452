-- A bigger example-------------


next :: Int -> Int
next 1 = 1
next n | even n    = div n 2
       | otherwise = n * 3 + 1


list :: Int -> [Int]
list n = toList [n]
    where
        toList :: [Int] -> [Int]
        toList ns@(x:xs) | x == 1    = ns
                         | otherwise = toList $ next x:ns

step :: Int -> Int
step  = length . list

steps :: Int -> Int
steps n
   | n == 1    = 1
   | otherwise = steps(next n)+1

numbers :: Int -> [Int]
numbers n
    | n==1      = [1]
    | otherwise = n : numbers(next n)
