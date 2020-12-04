data Space 
    = Tree 
    | Open deriving (Show, Eq)

type Row = [Space]

travelPath :: Int -> Int -> [Row] -> [Space]
travelPath dx dy rows =
    travelPath' 0 0 rows
    where
        travelPath' x y rows
            | y < length rows =
                rows !! y !! x : travelPath' (x + dx) (y + dy) rows
            | otherwise =
                []

charToSpace :: Char -> Space
charToSpace '.' = Open
charToSpace '#' = Tree
charToSpace _ = 
    error "Invalid input"

makeRow :: String -> [Space]
makeRow =
    cycle . fmap charToSpace

solve :: [Row] -> Int -> Int -> Int
solve rows x y =
    length . filter (== Tree) $ travelPath x y rows

main :: IO ()
main = do
    rows <- fmap makeRow . lines <$> readFile "input.txt"
    print $ solve rows 3 1
    print . product $ uncurry (solve rows) <$> [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
