import Text.Parsec
import Control.Monad.Identity ( Identity )

data PasswordRule = PasswordRule 
    { lowerBound :: Int
    , upperBound :: Int
    , requiredChar :: Char
    , password :: String
    }

{- Why this isn't a built-in is beyond me -}
maybeNth :: Int -> [a] -> Maybe a
maybeNth index xs
    | index >= length xs =
        Nothing
    | otherwise =
        Just (xs !! index)

parseRule :: ParsecT String u Identity PasswordRule
parseRule = do
    lowerBound <- read <$> many1 digit
    char '-'
    upperBound <- read <$> many1 digit
    space
    letter <- anyChar
    string ": "
    password <- many1 anyChar
    return $ PasswordRule lowerBound upperBound letter password

isRuleValid :: PasswordRule -> Bool
isRuleValid (PasswordRule lowerBound upperBound requiredChar password) =
    let 
        requiredCount = length $ filter (== requiredChar) password
    in
        requiredCount >= lowerBound && requiredCount <= upperBound

isRuleValidAlt :: PasswordRule -> Bool
isRuleValidAlt (PasswordRule lowerBound upperBound requiredChar password) =
    let
        exactlyOneTrueElement = (\xs -> length xs == 1) . filter (== True)
        fstCharIsRequiredChar = (== requiredChar) <$> maybeNth (lowerBound - 1) password
        sndCharIsRequiredChar = (== requiredChar) <$> maybeNth (upperBound - 1) password
    in
        maybe False exactlyOneTrueElement $ sequenceA [fstCharIsRequiredChar, sndCharIsRequiredChar]

parseAndValidate :: (PasswordRule -> Bool) -> String -> Either ParseError Bool
parseAndValidate validator ruleString =
    validator <$> parse parseRule "" ruleString

countValid :: [Bool] -> Int
countValid =
    length . filter (== True)

main :: IO ()
main = do
    passwords <- lines <$> readFile "input.txt"
    print $ countValid <$> mapM (parseAndValidate isRuleValid) passwords
    print $ countValid <$> mapM (parseAndValidate isRuleValidAlt) passwords
