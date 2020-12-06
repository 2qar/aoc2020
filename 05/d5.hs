import Data.List
import Text.Printf

partIter :: Bool -> [Int] -> [Int]
partIter h ns
    | not h = take t ns
    | h     = drop t ns
    where t = div ((last ns) - (head ns) + 1) 2

part :: Char -> [Int] -> String -> Int
part r ns s
    | length ns == 1 = head ns
    | otherwise      = part r (partIter h ns) (tail s)
    where h = (head s) == r

decode :: String -> Int
decode s = row * 8 + col
    where row = part 'B' [0..127] (take 7 s)
          col = part 'R' [0..7] (drop 7 s)

-- Assumes `ids` is sorted
missingID :: [Int] -> Int
missingID ids
    | null ids            = -1
    | last p - head p > 1 = last p - 1
    | otherwise           = missingID $ tail ids
    where p = take 2 ids

main = do
    ss <- getContents
    let ids = map decode $ lines ss

    printf "Silver: %d\n" $ maximum ids
    printf "Gold:   %d\n" $ missingID $ sort ids
