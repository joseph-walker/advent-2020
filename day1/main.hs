import Data.List

combinations :: [Int] -> [([Int], Int)]
combinations list =
    [([a, b], a + b) | a <- list , b <- list, a /= b]

combinations3 :: [Int] -> [([Int], Int)]
combinations3 list =
    [([a, b, c], a + b + c) | a <- list, b <- list, c <- list, a /= b && b /= c && c /= a]

first2020 :: [(a, Int)] -> Maybe a
first2020 xs =
    fst <$> find (\(_, sum) -> sum == 2020) xs

readInput :: IO [Int]
readInput = do
    input <- readFile "input.txt"
    return $ read <$> lines input

main :: IO ()
main = do
    list <- readInput
    print . fmap product . first2020 $ combinations list
    print . fmap product . first2020 $ combinations3 list
