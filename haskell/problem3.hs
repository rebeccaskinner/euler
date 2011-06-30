-- The prime factors of 13195 are 5, 7, 13 and 29.
-- What is the largest prime factor of the number 600851475143 ?
intSqrt = ceiling . sqrt . fromIntegral
isPrime n = let h = intSqrt n in
             [] == [x | x <- [2..h], n `mod` x == 0]
factors n = let h  = intSqrt n
                lf = [x | x <- [2..h], n `mod` x == 0] in
            map (div n) lf ++ reverse lf
biggestPrimeFactor = head . filter isPrime . factors
main = print $ biggestPrimeFactor 600851475143
