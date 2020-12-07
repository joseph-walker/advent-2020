import Data.List

data Split 
    = SplitLeft
    | SplitRight
    deriving (Show, Eq)

parseSplit :: Char -> Split
parseSplit 'F' = SplitLeft
parseSplit 'B' = SplitRight
parseSplit 'L' = SplitLeft
parseSplit 'R' = SplitRight
parseSplit _ =
    error "Undefined Input"

split :: Split -> [a] -> [a]
split _ [] =
    error "Empty List"
split _ [x] =
    [x]
split SplitLeft xs =
    take (length xs `div` 2) xs
split SplitRight xs =
    drop (length xs `div` 2) xs

solve :: String -> Int
solve input = 
    (row * 8) + col
    where
        row = head . solve [0..127] $ take 7 input
        col = head . solve [0..7] $ drop 7 input
        solve range xs = 
            foldl (flip split) range (parseSplit <$> xs)

main :: IO ()
main = do
    input <- fmap solve . lines <$> readFile "input.txt"
    print "Part 1"
    print . maximum $ input
    print "Part 2"
    print . solveForMissingId . sort $ input
    where
        solveForMissingId ids =
            (+1) . fst . head $ dropWhile (\(a, b) -> a == b - 1) (zip <*> tail $ ids)
