-- The prime factors of 13195 are 5, 7, 13 and 29.
-- What is the largest prime factor of the number 600851475143 ?

integer_sqrt = ceiling . sqrt . fromIntegral

is_prime n = let h = integer_sqrt n in
             [] == [x | x <- [2..h], n `mod` x == 0]

factors n = let h  = integer_sqrt n
                lf = [x | x <- [2..h], n `mod` x == 0] in
            (map (div n) lf) ++ (reverse lf)

biggest_prime_factor = head . (filter is_prime) . factors

main = do
    let bpf = biggest_prime_factor 600851475143
    putStrLn (show bpf)
