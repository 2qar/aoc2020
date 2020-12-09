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

sumRange :: [Int] -> [Int] -> Int -> [Int]
sumRange ns acc n
    | null ns      = []
    | sum acc > n  = []
    | sum acc == n = acc
    | otherwise    = sumRange (tail ns) ((head ns) : acc) n

invalidSum :: [Int] -> Int -> [Int]
invalidSum ns n
    | null ns      = []
    | not (null r) = r
    | otherwise    = invalidSum (tail ns) n
    where r = sumRange ns [] n

main = do
    s <- getContents
    let ns = map read $ lines s

    let invalid = firstInvalid ns
    printf "Silver: %d\n" invalid
    let ir = invalidSum ns invalid
    printf "Gold:   %d\n" ((minimum ir) + (maximum ir))
