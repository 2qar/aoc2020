import Text.Printf

type Policy = ([Int], Char, String)

xor :: Bool -> Bool -> Bool
xor c1 c2 = c1 /= c2

-- "1-2" -> [1,2]
range :: String -> [Int]
range s = map read $ words [if c == '-' then ' ' else c | c <- s]

parsePolicy :: String -> Policy
parsePolicy p = (range (i !! 0), head (i !! 1), i !! 2)
    where i = words p

valid1 :: Policy -> Bool
valid1 (r,m,s) = (head r) <= n && n <= last(r)
    where n = length $ filter (\c -> c == m) s

valid2 :: Policy -> Bool
valid2 (r,m,s) = sm `xor` em
    where sm = s !! ((head r) - 1) == m
          em = s !! ((last r) - 1) == m

solve :: (Policy -> Bool) -> [Policy] -> Int
solve valid ps = length 
                $ filter (\c -> c == True)
                $ map valid ps

main = do
    ss <- getContents
    let ps = map parsePolicy $ lines ss

    printf "Silver: %d\n" $ solve valid1 ps
    printf "Gold:   %d\n" $ solve valid2 ps
