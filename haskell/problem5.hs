-- 2520 is the smallest number that can be divided by each of the numbers from 1
-- to 10 without any remainder.
-- 
-- What is the smallest positive number that is evenly divisible by all of the
-- numbers from 1 to 20?
import Data.List

intSqrt = ceiling . sqrt . fromIntegral
isFactor x y = x `mod` y == 0
stripFactors (x:[]) = [x]
stripFactors (x:xs) = if or $ map (flip isFactor x) xs
                      then stripFactors xs
                      else x : stripFactors xs

checkDiv x y = and $ map (\n -> y `mod` n == 0) x

main = print $ head [x | x <- [20,40..], checkDiv (stripFactors [1..20]) x]
