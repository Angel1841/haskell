import Data.Char
main::IO()
main = do
    print $ sumSpecialPrimes 5 2 == 392 -- n = 5, d = 2
    print $ sumSpecialPrimes 5 3 == 107
    print $ sumSpecialPrimes 10 3 == 462

sumSpecialPrimes :: Int -> Int -> Int
sumSpecialPrimes n d = sum . take n . filter(checkIfDigitContained d) $ primes

checkIfDigitContained :: Int -> Int -> Bool
checkIfDigitContained d = any (== intToDigit d) . show
 
primes :: [Int]
primes = filter isPrime[2..]

isPrime :: Int -> Bool
isPrime n = [1, n] == filter (\ d -> mod n d == 0) [1 .. n] 