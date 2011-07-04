-- The sum of the squares of the first ten natural numbers is,
-- 
-- 12 + 22 + ... + 102 = 385
-- The square of the sum of the first ten natural numbers is,
-- 
-- (1 + 2 + ... + 10)2 = 552 = 3025
-- Hence the difference between the sum of the squares of the first ten natural
-- numbers and the square of the sum is 3025  385 = 2640.
-- 
-- Find the difference between the sum of the squares of the first one hundred
-- natural numbers and the square of the sum.

sumOfSquares :: Integer -> Integer
sumOfSquares max = sum $ map (^2) [1..max]

squareOfSum :: Integer -> Integer
squareOfSum max = sum [1..max] ^ 2

-- exploit the fact that (a+b+c+d..)^2 - (a^2+b^2+c^2+d^2) =
-- a(2b + 2c + 2d) + b(2c + 2d) + c(2d) =
-- 2ab + 2ac + 2ad + 2bc + 2bd + 2cd
fastDifference a b = sum $ concatMap (\x -> map (\y -> (2*x*y)) [(x+1)..b]) [a..(b-1)]
slowDifference n = squareOfSum n - sumOfSquares n

main = do
    let slow = slowDifference 100
    let fast = fastDifference 1 100
    putStrLn ("Fast: " ++ (show fast))
    putStrLn ("Slow: " ++ (show slow))
