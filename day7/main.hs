import Text.Parsec
import Control.Monad.Identity ( Identity )
import qualified Data.Map as Map

(!) = (Map.!)

type BagRules = Map.Map String (Maybe (Map.Map String Int))

pColor :: ParsecT String u Identity String
pColor = do
    adj <- many1 letter
    space
    col <- many1 letter
    return $ adj ++ " " ++ col

pColorRule :: ParsecT String u Identity (String, Int)
pColorRule = do
    num <- read <$> many1 digit
    space
    col <- pColor
    string " bag"
    optional (char 's')
    return (col, num)

pBagRule :: ParsecT String u Identity (String, Maybe (Map.Map String Int))
pBagRule = do
    dstBag <- pColor
    string " bags contain "
    srcBag <- try noOtherBags <|> justSrcBags
    char '.'
    return (dstBag, Map.fromList <$> srcBag)
    where
        justSrcBags = Just <$> pColorRule `sepBy` string ", "
        noOtherBags = Nothing <$ string "no other bags"

pInput :: ParsecT String u Identity BagRules
pInput = 
    Map.fromList <$> pBagRule `sepBy` string "\n"

traverseRulesPtA :: BagRules -> String -> Bool
traverseRulesPtA rules entry =
    case rule of
        Just rule' ->
            Map.member "shiny gold" rule' || or (traverseRulesPtA rules <$> Map.keys rule')
        Nothing ->
            False
    where
        rule = rules ! entry

main :: IO ()
main = do
    input <- parse pInput "" <$> readFile "input.txt"
    print "Part 1"
    case input of
        Left _ -> error "Invalid Input"
        Right rules ->
            print . length . filter (== True ) $ traverseRulesPtA rules <$> Map.keys rules
    print "Part 2"
