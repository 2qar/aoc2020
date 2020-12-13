import Data.List
import Data.Maybe
import Text.Printf

parseNums :: String -> [Int]
parseNums s
    | null s          = []
    | (head s) == 'x' = parseNums t
    | otherwise       = [n] ++ (parseNums t)
    where n = read $ takeWhile (\c -> c /= ',') s
          t = drop 1 $ dropWhile (\c -> c /= ',') s

earliestID :: Int -> [Int] -> (Int, Int)
earliestID ts ids
    | isJust i  = (ts, ids !! (fromJust i))
    | otherwise = earliestID (ts + 1) ids
    where i = findIndex (==0) $ map (\id -> ts `mod` id) ids

main = do
    s  <- getLine
    ss <- getLine
    let n = read s::Int
    let ns = parseNums ss
    let earliest = earliestID n ns

    printf "Silver: %d\n" $ ((fst earliest) - n) * (snd earliest)
