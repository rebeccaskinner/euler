-- The prime factors of 13195 are 5, 7, 13 and 29.
-- What is the largest prime factor of the number 600851475143 ?

--factors n = [ x | x <- [1..n-1], (x `mod` n) == 0]
factors n = [ x | x <- reverse [1..n-1], odd x, n `mod` x == 0 ]
empty = (==0) . length
is_prime n = empty ( filter (/=1) (factors n))

main = do
    --let biggestFactor = head (filter is_prime (reverse . factors 600851475143))
    let facts = reverse $ factors 600851475143
    let biggestFactor = head ( filter is_prime facts )
    putStrLn (show biggestFactor)
