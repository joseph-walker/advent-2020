combinations :: [Int] -> Int
combinations list =
    head $ [a * b |
        a <- list, 
        b <- list, 
        a /= b, 
        a + b == 2020
        ]

combinations3 :: [Int] -> Int
combinations3 list =
    head $ [a * b * c |
        a <- list, 
        b <- list, 
        c <- list, 
        a /= b && b /= c && c /= a, 
        a + b + c == 2020
        ]

readInput :: IO [Int]
readInput = do
    input <- readFile "input.txt"
    return $ read <$> lines input

main :: IO ()
main = do
    list <- readInput
    print $ combinations list
    print $ combinations3 list
