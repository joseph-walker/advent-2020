import Data.List
import Data.List.Split ( splitOn )

solve :: [String] -> Int
solve input =
    let 
        numInputs = length input
        countedInputs = 
            fmap length . group . sort . concat $ input
    in
        length $ filter (== numInputs) countedInputs

main :: IO ()
main = do
    input <- splitOn "\n\n" <$> readFile "input.txt"
    print "Part 1"
    print . sum $ length . fmap head . group . sort . concat . lines <$> input
    print "Part 2"
    print . sum $ solve . lines <$> input
