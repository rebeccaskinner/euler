-- A palindromic number reads the same both ways. The largest palindrome made from
-- the product of two 2-digit numbers is 9009 = 91  99.
-- 
-- Find the largest palindrome made from the product of two 3-digit numbers.

import Data.List

isPalindrome n = show n == reverse (show n)
products = concatMap (\x -> map (*x) [999,998..100]) [999,998..100]
main = print $ head (filter isPalindrome (reverse (sort products)))
