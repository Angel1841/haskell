main :: IO()
main = do
    print $ sumUnevenLC 5 50 == 621
    print $ sumUnevenLC 50 1 == 625
    print $ sumUnevenLC 564 565 == 565
    print $ sumUnevenLC 8 11 == 20 -- my test

    print $ sumUnevenHOF 5 50 == 621
    print $ sumUnevenHOF 50 1 == 625
    print $ sumUnevenHOF 564 565 == 565
    print $ sumUnevenHOF 5 3 == 8 -- my test

sumUnevenLC :: Int -> Int -> Int
sumUnevenLC n a = sum [x | x <- [min n a .. max n a], mod x 2 /= 0]

sumUnevenHOF :: Int -> Int -> Int
sumUnevenHOF n a = sum $ filter (\ x -> odd x ) [min n a .. max n a]
