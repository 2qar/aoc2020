import Data.List
import Text.Printf

partIter :: Int -> [Int] -> [Int]
partIter h ns
    | h == 0 = take t ns
    | h == 1 = drop t ns
    where t = div ((last ns) - (head ns) + 1) 2

part :: Char -> [Int] -> String -> Int
part r ns s
    | length ns == 1 = head ns
    | otherwise      = part r (partIter h ns) (tail s)
    where h = fromEnum $ (head s) == r

decode :: String -> Int
decode s = row * 8 + col
    where row = part 'B' [0..127] (take 7 s)
          col = part 'R' [0..7] (drop 7 s)

findID :: Int -> [Int] -> Int
findID last ids
    | null ids  = -1
    | last == 0 = findID c (tail ids)
    | c - last > 1 = c - 1
    | otherwise = findID c (tail ids)
    where c = head ids

main = do
    ss <- getContents
    let ids = map decode $ lines ss

    printf "Silver: %d\n" $ maximum ids
    printf "Gold:   %d\n" $ findID 0 $ sort ids
