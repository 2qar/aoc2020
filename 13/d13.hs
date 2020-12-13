import Data.List
import Data.Maybe
import GHC.Integer
import Text.Printf

parseNums :: String -> [Int]
parseNums s
    | null s          = []
    | (head s) == 'x' = [-1] ++ (parseNums t)
    | otherwise       = [n] ++ (parseNums t)
    where n = read $ takeWhile (\c -> c /= ',') s
          t = drop 1 $ dropWhile (\c -> c /= ',') s

earliestID :: Int -> [Int] -> (Int, Int)
earliestID ts ids
    | isJust i  = (ts, ids !! (fromJust i))
    | otherwise = earliestID (ts + 1) ids
    where i = findIndex (==0) $ map (\id -> ts `mod` id) ids

validSequenceIter :: Int -> Integer -> [Int] -> Bool
validSequenceIter i id ids
    | i == (length ids) = True
    | (ids !! i) == -1              = validSequenceIter (i + 1) id ids
    | otherwise                     = valid && (validSequenceIter (i + 1) id ids)
    where valid = (modInteger (id + (toInteger i)) $ toInteger (ids !! i)) == 0

validSequence :: Integer -> [Int] -> Bool
validSequence id ids = validSequenceIter 0 id ids

earliestValidIter :: Integer -> Integer -> [Int] -> Integer
earliestValidIter i step ids
    | valid     = i
    | otherwise = earliestValidIter (i + step) step ids
    where valid = validSequence i ids

earliestValid :: [Int] -> Integer
earliestValid ids = earliestValidIter 0 (toInteger $ head ids) ids

main = do
    s  <- getLine
    ss <- getLine
    let n = read s::Int
    let ns = parseNums ss
    let earliest = earliestID n $ filter (\id -> id /= (-1)) ns

    printf "Silver: %d\n" $ ((fst earliest) - n) * (snd earliest)
    printf "Gold:   %d\n" $ earliestValid ns
