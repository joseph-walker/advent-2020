import Data.List
import Data.List.Split ( splitOn )

main :: IO ()
main = do
    input <- splitOn "\n\n" <$> readFile "input.txt"
    print "Part 1"
    print . sum $ length . fmap head . group . sort . concat . lines <$> input
