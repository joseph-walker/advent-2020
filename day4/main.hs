import Text.Parsec
import qualified Data.Map as Map
import Data.List.Split ( splitOn )
import Control.Monad.Identity ( Identity )

type Passport = Map.Map String String

data Height
    = In Int
    | Cm Int deriving (Show, Eq)

pKeyValPairGeneric :: ParsecT String u Identity (String, String)
pKeyValPairGeneric = do
    key <- count 3 letter
    char ':'
    val <- many1 (alphaNum <|> char '#')
    return (key, val)

makepYear :: String -> Int -> Int -> ParsecT String u Identity (String, Maybe Int)
makepYear key low high = do
    key' <- string key <* char ':'
    val <- fmap read <$> optionMaybe (try $ count 4 digit)
    return (key', val >>= whereValid)
    where
        whereValid val'
            | val' < low = Nothing
            | val' > high = Nothing
            | otherwise = Just val'

pBirthYear :: ParsecT String u Identity (String, Maybe Int)
pBirthYear = makepYear "byr" 1920 2002

pIssueYear :: ParsecT String u Identity (String, Maybe Int)
pIssueYear = makepYear "iyr" 2010 2020

pExpireYear :: ParsecT String u Identity (String, Maybe Int)
pExpireYear = makepYear "eyr" 2020 2030

pHeight :: ParsecT String u Identity (String, Maybe Height)
pHeight = do
    key <- string "hgt" <* char ':'
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
            | otherwise = Just hgt
        whereValid hgt@(Cm val')
            | val' < 150 = Nothing
            | val' > 193 = Nothing
            | otherwise = Just hgt

pEyeColor :: ParsecT String u Identity (String, Maybe String)
pEyeColor = do
    key <- string "ecl" <* char ':'
    val <- optionMaybe (try validEyeColor)
    return (key, val)
    where
        validEyeColor = 
            string "amb" <|>
            string "blu" <|>
            string "brn" <|>
            string "gry" <|>
            string "grn" <|>
            string "hzl" <|>
            string "oth"

pHairColor :: ParsecT String u Identity (String, Maybe String)
pHairColor = do
    key <- string "hcl" <* char ':'
    val <- optionMaybe (try $ char '#' *> count 6 (oneOf "abcdef" <|> digit))
    return (key, (:) '#' <$> val)

pPassportId :: ParsecT String u Identity (String, Maybe String)
pPassportId = do
    key <- string "pid" <* char ':'
    val <- optionMaybe (try $ count 9 digit)
    return (key, val)

pPassportGeneric :: ParsecT String u Identity Passport
pPassportGeneric = 
    Map.fromList <$> pKeyValPairGeneric `sepBy` space

isPassportValidGeneric :: Passport -> Bool
isPassportValidGeneric p
    | Map.size p == 8 = True
    | Map.size p == 7 && Map.notMember "cid" p = True
    | otherwise = False

main :: IO ()
main = do
    file <- splitOn "\n\n" <$> readFile "input.txt"
    case sequenceA $ parse pPassportGeneric "" <$> file of
        Left _ ->
            error "Invalid Input"
        Right passports ->
            print $ length . filter (== True) $ isPassportValidGeneric <$> passports
