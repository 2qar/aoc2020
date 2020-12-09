import Data.List
import Control.Monad
import Text.Printf

valid :: [[Int]] -> Int -> Bool
valid ns i = length (filter (\s -> s == i) $ map sum ns) > 0

combos :: [Int] -> [[Int]]
combos = filter (\l -> (length $ nub l) == 2) . replicateM 2

firstInvalid :: [Int] -> Int
firstInvalid ns
    | null ns          = -1
    | not (valid cs n) = n
    | otherwise        = firstInvalid $ tail ns
    where cs = combos $ take 25 ns
          n  = head $ drop 25 ns

main = do
    s <- getContents
    let ns = map read $ lines s

    printf "Silver: %d\n" $ firstInvalid ns
