import Data.List
import Data.Maybe
import qualified Data.Set as Set
import Text.Printf

type Rule = (String, [Int])

parseRange :: String -> [Int]
parseRange s = [n1..n2]
    where n1 = read $ takeWhile (\c -> c /= '-') s
          n2 = read $ drop 1 $ dropWhile (\c -> c /= '-') s

-- "depar...: 41-525 or 538-969" -> [41..525] ++ [538..969]
parseRule :: String -> Rule
parseRule s = (f, (parseRange (rs !! 0)) ++ (parseRange (rs !! 2)))
    where f  = takeWhile (\c -> c /= ':') s
          rs = words $ drop 2 $ dropWhile (\c -> c /= ':') s

-- skip to the next set of lines after a blank line
nextLines :: [String] -> [String]
nextLines = drop 1 . dropWhile (\l -> not (null l))

currentLines :: [String] -> [String]
currentLines = takeWhile (\l -> not (null l))

parseLine :: String -> [Int]
parseLine s
    | null s    = []
    | otherwise = [i] ++ (parseLine n)
    where i = read $ takeWhile (\c -> c /= ',') s
          n = drop 1 $ dropWhile (\c -> c /= ',') s

invalid :: Set.Set Int -> Int -> Int
invalid is i
    | Set.member i is = 0
    | otherwise       = i

validTickets :: Set.Set Int -> [[Int]] -> [[Int]]
validTickets is ts = filter (\t -> (sum $ map (\i -> invalid is i) t) == 0) ts

rulesFor :: [Int] -> [Rule] -> [String]
rulesFor is rs
    | null is   = map fst rs
    | otherwise = rulesFor (tail is) $ filter (\r -> elem i $ snd r) rs
    where i = head is

removeRule :: String -> (Int, [String]) -> [String]
removeRule r p
    | (fst p) <= 1 = snd p
    | otherwise    = delete r $ snd p

rulesForAll :: [(Int, [String])] -> [(Int, [String])]
rulesForAll rs
    | isNothing r = rs
    | otherwise   = rulesForAll $ map (\p -> ((fst p) - 1, removeRule (head $ snd $ fromJust r) p)) rs
    where r = find (\p -> (fst p) == 1) rs

fields :: [[String]] -> [String]
fields fs = map (\p -> head $ snd p) $ rulesForAll $ zip (map length fs) fs

main = do
    s <- getContents
    let ls = lines s
    let rs = map parseRule $ currentLines ls
    let ts = map parseLine $ drop 1 $ nextLines $ nextLines ls

    let is = Set.fromList $ foldl1 (++) $ map snd rs
    printf "Silver: %d\n" $ sum $ map (\l -> sum $ map (\i -> invalid is i) l) ts

    let t   = parseLine $ head $ drop 1 $ currentLines $ nextLines ls
    let vts = (validTickets is ts)
    let fs  = map (\is -> rulesFor is rs) $ transpose vts
    printf "Gold:   %d\n" $ product $ map fst $ filter (\p -> isPrefixOf "departure" (snd p)) $ zip t $ fields fs

