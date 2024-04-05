main :: IO()
main = do
    print $ isPresentRecNonPM 0 [] == False
    print $ isPresentRecNonPM 0 [1, 2, 3] == False
    print $ isPresentRecNonPM 0 [0, -1, 2] == True
    print $ isPresentRecNonPM 4 [0, 4, 6, 7] == True -- my test


    print $ isPresentRecPM 0 [] == False
    print $ isPresentRecPM 0 [1, 2, 3] == False
    print $ isPresentRecPM 0 [0, -1, 2] == True
    print $ isPresentRecPM 3 [1, 0, 2] == False -- my test
    

    print $ isPresentFunc 0 [] == False
    print $ isPresentFunc 0 [1, 2, 3] == False
    print $ isPresentFunc 0 [0, -1, 2] == True
    print $ isPresentFunc 2 [2, 3, 4, 5, 6] == True -- my test

isPresentFunc :: Int -> [Int] -> Bool
isPresentFunc n xs = elem n xs

isPresentRecPM :: Int -> [Int] -> Bool
isPresentRecPM _ [] = False
isPresentRecPM n (x:xs)
    | n == x = True
    | otherwise = isPresentRecPM n xs

isPresentRecNonPM :: Int -> [Int] -> Bool
isPresentRecNonPM n xs
    | null xs = False
    | head xs == n = True
    | otherwise = isPresentRecNonPM n (tail xs)
