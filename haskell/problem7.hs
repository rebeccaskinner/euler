-- By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that
-- the 6th prime is 13.
-- 
-- What is the 10001st prime number?
primes [] = []
primes (p:[]) = [p]
primes (p:xs) = p : primes [x | x<-xs, x `mod` p > 0]
main = print $ primes [2..] !! 10000
