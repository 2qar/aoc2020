import Data.List
import Text.Printf

nextJolt :: Int -> [Int] -> Int
nextJolt n ns
    | null ns   = n + 3
    | otherwise = (head ns)

diffs :: [Int] -> [Int]
diffs ns
    | null ns   = []
    | otherwise = ((nextJolt n nns) - n) : (diffs $ nns)
    where n   = head ns
          nns = tail ns

main = do
    s <- getContents
    let ns = 0 : (sort $ map read $ lines s)

    let ds = group $ sort $ diffs ns
    printf "Silver: %d\n" $ (length $ head ds) * (length $ last ds)
