import Data.List
import Data.Maybe
import qualified Data.Map.Strict as Map
import Text.Printf

type Rule = [(Int, String)]
type Rules = Map.Map String Rule

hasSep :: String -> Bool
hasSep w = (elem ',' w) || (elem '.' w)

parseRules :: [String] -> [(Int, String)]
parseRules ws
    | null ws           = []
    | (head ws) == "no" = []
    | otherwise         = [(read i, unwords r)] ++ (parseRules n)
    where i = head ws
          r = takeWhile (\w -> not $ hasSep w) $ drop 1 ws
          n = drop 1 $ dropWhile (\w -> not $ hasSep w) ws

parseRule :: [String] -> (String, Rule)
parseRule ws = (n, parseRules rs)
    where n  = unwords $ take 2 $ takeWhile (\l -> l /= "contain") ws
          rs = drop 1 $ dropWhile (\w -> w /= "contain") ws

findAndCheck :: Rules -> String -> Bool
findAndCheck rs n = maybe False (\r -> hasShinyGold rs r) $ Map.lookup n rs

hasShinyGold :: Rules -> Rule -> Bool
hasShinyGold rs r 
    | elem "shiny gold" $ map snd r = True
    | otherwise                     = elem True $ map (\r -> findAndCheck rs $ snd r) r

findRule :: Rules -> String -> Rule
findRule rs n = maybe [] (\r -> r) $ Map.lookup n rs

bagsIn :: Rules -> Rule -> Int
bagsIn rs r
    | null r    = 0
    | otherwise = sum $ map (\r -> (fst r) + (fst r) * (bagsIn rs $ findRule rs $ snd r)) r

main = do
    s <- getContents
    let rules = Map.fromList $ map (\l -> parseRule $ words l) $ lines s

    printf "Silver: %d\n" $ sum $ Map.map (\r -> fromEnum $ hasShinyGold rules r) rules
    printf "Gold:   %d\n" $ bagsIn rules $ fromJust $ Map.lookup "shiny gold" rules
