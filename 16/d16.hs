import qualified Data.Set as Set
import Text.Printf

parseRange :: String -> [Int]
parseRange s = [n1..n2]
    where n1 = read $ takeWhile (\c -> c /= '-') s
          n2 = read $ drop 1 $ dropWhile (\c -> c /= '-') s

-- "depar...: 41-525 or 538-969" -> [41..525] ++ [538..969]
parseRule :: String -> [Int]
parseRule s = (parseRange (rs !! 0)) ++ (parseRange (rs !! 2))
    where rs = words $ drop 2 $ dropWhile (\c -> c /= ':') s

-- skip to the next set of lines after a blank line
nextLines :: [String] -> [String]
nextLines = drop 1 . dropWhile (\l -> not (null l))

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

main = do
    s <- getContents
    let ls = lines s
    let is = Set.fromList $ foldl1 (++) $ map parseRule $ takeWhile (\l -> not (null l)) ls
    let ts = map parseLine $ drop 1 $ nextLines $ nextLines ls

    printf "Silver: %d\n" $ sum $ map (\l -> sum $ map (\i -> invalid is i) l) ts
