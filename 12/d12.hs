import Text.Printf

angle :: Int -> Int
angle a
    | a == 360  = 0
    | a > 360   = a - 360
    | a < 0     = 360 + a
    | otherwise = a

forward :: (Int, Int, Int) -> Int -> (Int, Int, Int)
forward (x, y, a) n
    | a == 0    = (x+n, y, a)
    | a == 90   = (x, y+n, a)
    | a == 180  = (x-n, y, a)
    | a == 270  = (x, y-n, a)
    | otherwise = (x, y, a)

move :: (Int, Int, Int) -> String -> (Int, Int, Int)
move (x, y, a) s
    | c == 'N'  = (x, y+n, a)
    | c == 'S'  = (x, y-n, a)
    | c == 'E'  = (x+n, y, a)
    | c == 'W'  = (x-n, y, a)
    | c == 'L'  = (x, y, angle (a+n))
    | c == 'R'  = (x, y, angle (a-n))
    | c == 'F'  = forward (x, y, a) n
    | otherwise = (x, y, a)
    where c = head s
          n = read $ tail s

dist :: (Int, Int, Int) -> Int
dist (x, y, _) = (abs x) + (abs y)

main = do
    s <- getContents
    let ss = lines s

    printf "Silver: %d\n" $ dist $ foldl (\r s -> move r s) (0, 0, 0) ss
