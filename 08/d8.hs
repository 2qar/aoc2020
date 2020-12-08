import Text.Printf

parseInt :: String -> Int
parseInt s
    | c == '-'  = i * (-1)
    | otherwise = i
    where c = head s
          i = read $ tail s

run :: Int -> Int -> [Int] -> [String] -> Int
run acc idx history is
    | elem idx history = acc
    | n == "acc"       = run (acc+i) (idx+1) (idx : history) is
    | n == "jmp"       = run acc (idx+i) (idx : history) is
    | n == "nop"       = run acc (idx+1) (idx : history) is
    | otherwise        = -1
    where c = is !! idx
          n = head $ words (is !! idx)
	  i = parseInt $ last $ words (is !! idx)

main = do
    s <- getContents

    printf "Silver: %d\n" $ run 0 0 [] $ lines s
