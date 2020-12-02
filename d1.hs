import Control.Monad

solve :: Int -> [Int] -> Int
solve n ns = product 
                $ head
		$ filter (\l -> (sum l) == 2020)
		$ replicateM n ns

main = interact $ show . solve 2 . map read . lines
