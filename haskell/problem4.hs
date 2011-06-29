import Data.List

isPalindrome n = show n == reverse (show n)
products = concatMap (\x -> map (*x) [999,998..100]) [999,998..100]
main = print $ head (filter isPalindrome (reverse (sort products)))
