import Data.List
import Data.Maybe
import Text.Printf

type Rule = (String, [(Int, String)])

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

parseRule :: [String] -> Rule
parseRule ws = (n, parseRules rs)
    where n  = unwords $ take 2 $ takeWhile (\l -> l /= "contain") ws
          rs = drop 1 $ dropWhile (\w -> w /= "contain") ws

findAndCheck :: [Rule] -> String -> Bool
findAndCheck rs n = maybe False (\n -> hasShinyGold rs n) $ find (\r -> (fst r) == n) rs

hasShinyGold :: [Rule] -> Rule -> Bool
hasShinyGold rs r 
    | elem "shiny gold" $ map snd $ snd r = True
    | otherwise                           = elem True $ map (\r -> findAndCheck rs $ snd r) $ snd r

findRule :: [Rule] -> String -> Rule
findRule rs n = maybe ("", []) (\r -> r) $ find (\r -> (fst r) == n) rs

bagsIn :: [Rule] -> Rule -> Int
bagsIn rs r
    | null nrs  = 0
    | otherwise = sum $ map (\r -> (fst r) + (fst r) * (bagsIn rs $ findRule rs $ snd r)) nrs
    where nrs = snd r

main = do
    s <- getContents
    let rules = map (\l -> parseRule $ words l) $ lines s

    printf "Silver: %d\n" $ sum $ map (\r -> fromEnum $ hasShinyGold rules r) rules
    let shiny = fromJust $ find (\r -> fst r == "shiny gold") rules
    printf "Gold:   %d\n" $ bagsIn rules shiny
