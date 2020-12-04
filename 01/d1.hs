import Text.Printf
import Control.Monad

solve :: Int -> [Int] -> Int
solve n ns = product 
                $ head
                $ filter (\l -> (sum l) == 2020)
                $ replicateM n ns

main = do
    ss <- getContents
    let ns = [read n | n <- (lines ss)]

    printf "Silver: %d\n" $ solve 2 ns
    printf "Gold:   %d\n" $ solve 3 ns
