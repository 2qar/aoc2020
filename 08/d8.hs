import Data.List
import Text.Printf

type Game = (Int, Int, [Int], [String])

parseInt :: String -> Int
parseInt s
    | c == '-'  = i * (-1)
    | otherwise = i
    where c = head s
          i = read $ tail s

gameAcc :: Game -> Int
gameAcc (acc, _, _, _) = acc

gameIdx :: Game -> Int
gameIdx (_, idx, _, _) = idx

runIter :: Game -> Game
runIter (acc, idx, history, is)
    | elem idx history         = (acc, -1, history, is)
    | idx == l && n == "acc"   = (acc+i, -2, history, is)
    | idx == l                 = (acc, -2, history, is)
    | n == "acc"               = runIter (acc+i, idx+1, idx : history, is)
    | n == "jmp"               = runIter (acc, idx+i, idx : history, is)
    | n == "nop"               = runIter (acc, idx+1, idx : history, is)
    | otherwise                = (-1, -1, [], [])
    where c = is !! idx
          n = take 3 (is !! idx)
          i = parseInt $ last $ words (is !! idx)
          l = (length is) - 1

run :: [String] -> Game
run is = runIter (0, 0, [], is)

replace :: [String] -> Int -> String -> [String]
replace ss n s = (fst split) ++ (i : (drop 1 $ snd split))
    where split = splitAt n ss
          i = s ++ (drop 3 $ head $ snd split)

perms :: Int -> [String] -> [[String]]
perms i is
    | i == (length is)           = []
    | isPrefixOf "jmp" (is !! i) = [(replace is i "nop")] ++ (perms (i+1) is)
    | isPrefixOf "nop" (is !! i) = [(replace is i "jmp")] ++ (perms (i+1) is)
    | otherwise                  = perms (i+1) is

main = do
    s <- getContents
    let is = lines s

    printf "Silver: %d\n" $ gameAcc $ run is
    let ps = is : (perms 0 is)
    printf "Gold: %d\n" $ gameAcc $ head $ filter (\g -> (gameIdx g) == -2) $ map run ps
