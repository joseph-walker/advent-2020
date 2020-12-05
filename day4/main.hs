import Text.Parsec
import qualified Data.Map as Map
import Data.List.Split ( splitOn )
import Control.Monad.Identity ( Identity )

type Passport = Map.Map String String

type PassportParser u = ParsecT String u Identity (String, Maybe PassportValue)

data Measure
    = In Int
    | Cm Int
    deriving (Show, Eq)

data PassportValue
    = Year Int
    | Hex String
    | Color String
    | Height Measure
    | Number String
    deriving (Show, Eq)

pKeyValPairGeneric :: ParsecT String u Identity (String, String)
pKeyValPairGeneric = do
    key <- count 3 letter
    char ':'
    val <- many1 (alphaNum <|> char '#')
    return (key, val)

pKey :: String -> ParsecT String u Identity String
pKey k = 
    string k <* char ':'

makepYear :: String -> Int -> Int -> PassportParser u
makepYear key low high = do
    key' <- pKey key
    val <- fmap read <$> optionMaybe (try $ count 4 digit)
    return (key', val >>= whereValid)
    where
        whereValid val'
            | val' < low = Nothing
            | val' > high = Nothing
            | otherwise = Just (Year val')

pBirthYear :: PassportParser u
pBirthYear = makepYear "byr" 1920 2002

pIssueYear :: PassportParser u
pIssueYear = makepYear "iyr" 2010 2020

pExpireYear :: PassportParser u
pExpireYear = makepYear "eyr" 2020 2030

pHeight :: PassportParser u
pHeight = do
    key <- pKey "hgt"
    val <- optionMaybe (try inches <|> try centimeters)
    return (key, val >>= whereValid)
    where
        inches = 
            In . read <$> many1 digit <* string "in"
        centimeters = 
            Cm . read <$> many1 digit <* string "cm"
        whereValid hgt@(In val')
            | val' < 59 = Nothing
            | val' > 76 = Nothing
            | otherwise = Just (Height hgt)
        whereValid hgt@(Cm val')
            | val' < 150 = Nothing
            | val' > 193 = Nothing
            | otherwise = Just (Height hgt)

pEyeColor :: PassportParser u
pEyeColor = do
    key <- pKey "ecl"
    val <- optionMaybe (try validEyeColor)
    return (key, Color <$> val)
    where
        validEyeColor = choice
            [ string "amb"
            , string "blu"
            , string "brn"
            , string "gry"
            , string "grn"
            , string "hzl"
            , string "oth"
            ]

pHairColor :: PassportParser u
pHairColor = do
    key <- pKey "hcl"
    val <- optionMaybe (try $ char '#' *> count 6 (oneOf "abcdef" <|> digit))
    return (key, Hex . (:) '#' <$> val)

pPassportId :: PassportParser u
pPassportId = do
    key <- pKey "pid"
    val <- optionMaybe (try $ count 9 digit)
    return (key, Number <$> val)

pCountryId :: PassportParser u
pCountryId = do
    key <- pKey "cid"
    val <- optionMaybe (try $ many1 digit)
    return (key, Number <$> val)

pPassportGeneric :: ParsecT String u Identity Passport
pPassportGeneric = 
    Map.fromList <$> pKeyValPairGeneric `sepBy` space

pPassport :: ParsecT String u Identity [(String, Maybe PassportValue)]
pPassport =
    pPassportEntry `sepBy` space
    where
        pPassportEntry = choice
            [ try pBirthYear
            , try pIssueYear
            , try pExpireYear
            , try pHeight
            , try pEyeColor
            , try pHairColor
            , try pPassportId
            , pCountryId
            ]

isPassportValidGeneric :: Passport -> Bool
isPassportValidGeneric p
    | Map.size p == 8 = True
    | Map.size p == 7 && Map.notMember "cid" p = True
    | otherwise = False

solveA :: IO ()
solveA = do
    print "Part 1"
    file <- splitOn "\n\n" <$> readFile "input.txt"
    case sequenceA $ parse pPassportGeneric "" <$> file of
        Left _ ->
            error "Invalid Input"
        Right passports ->
            print $ length . filter (== True) $ isPassportValidGeneric <$> passports

solveB :: IO ()
solveB = do
    print "Part 2"
    file <- splitOn "\n\n" <$> readFile "input.txt"
    print $ parse pPassport "" <$> file

main :: IO ()
main = do
    solveA
    solveB