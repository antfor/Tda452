

f :: Double -> Int -> [Double]
f d e = h d e 1 []

h :: Double -> Int -> Double ->[Double]-> [Double]
h d e n l |  ((d - c ) < (1 / (10^^(e)))) = l
          | nn > d = h d e (n+1) l
          | otherwise = h d e (n+1) (n:l)
  where c = sum' l
        nn = 1/n + c

sum' [] = 0
sum' (n:ns) = 1/n + sum' ns
