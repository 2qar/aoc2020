import Data.List
import Data.Maybe
import Text.Printf
-- tree: light red    -> (bright white, muted yellow)
--       bright white -> (shiny gold, none)
--       muted yellow -> (shiny gold, faded blue)
--       faded blue   -> (none, none)

type Rule = (String, [String])

hasSep :: String -> Bool
hasSep w = (elem ',' w) || (elem '.' w)

parseRules :: [String] -> [String]
parseRules ws
    | null ws           = []
    | (head ws) == "no" = []
    | otherwise         = [(unwords r)] ++ (parseRules n)
    where r = takeWhile (\w -> not $ hasSep w) $ drop 1 ws
          n = drop 1 $ dropWhile (\w -> not $ hasSep w) ws

parseRule :: [String] -> Rule
parseRule ws = (n, parseRules rs)
    where n  = unwords $ take 2 $ takeWhile (\l -> l /= "contain") ws
          rs = drop 1 $ dropWhile (\w -> w /= "contain") ws

findAndCheck :: [Rule] -> String -> Bool
findAndCheck rs n = maybe False (\n -> hasShinyGold rs n) $ find (\r -> (fst r) == n) rs

hasShinyGold :: [Rule] -> Rule -> Bool
hasShinyGold rs r 
    | elem "shiny gold" $ snd r = True
    | otherwise                 = elem True $ map (\n -> findAndCheck rs n) $ snd r

main = do
    s <- getContents
    let rules = map (\l -> parseRule $ words l) $ lines s
    print rules

    printf "Silver: %d\n" $ sum $ map (\r -> fromEnum $ hasShinyGold rules r) rules
