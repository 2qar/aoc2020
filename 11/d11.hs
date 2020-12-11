-- black magic garbage
neighborsIter :: Int -> (Int, Int) -> [String] -> Int
neighborsIter i (x0, y0) ss
    | i == 9           = 0
    | i == 4           = 0 + neighborsIter (i+1) (x0, y0) ss
    | x < 0 || x >= xl = 0 + neighborsIter (i+1) (x0, y0) ss
    | y < 0 || y >= yl = 0 + neighborsIter (i+1) (x0, y0) ss
    | otherwise        = (fromEnum $ ((ss !! y) !! x) == '#') + neighborsIter (i+1) (x0, y0) ss
    where x = x0 + (i `mod` 3)
          y = y0 + (i `div` 3)
          xl = length $ head ss
          yl = length ss

occupiedNeighbors :: (Int, Int) -> [String] -> Int
occupiedNeighbors (x, y) = neighborsIter 0 (x-1, y-1)

nextState :: Char -> Int -> Char
nextState c os
    | c == '.'  = '.'
    | os == 0   = '#'
    | os >= 4   = 'L'
    | otherwise = c

stepIter :: (Int, Int) -> [String] -> [String] -> [String]
stepIter (x, y) ns ss
    | y == (length ss)        = init ns
    | x == (length $ head ss) = stepIter (0, y+1) (ns ++ [""]) ss
    | otherwise               = stepIter (x+1, y) (ls ++ [l ++ [nextState ((ss !! y) !! x) os]]) ss
    where os = occupiedNeighbors (x,y) ss
          ls = init ns
          l  = last ns

step :: [String] -> [String]
step = stepIter (0, 0) [""]

stabilizedSeats :: [String] -> Int
stabilizedSeats ss
    | ns == ss  = sum $ map (\l -> length $ filter (=='#') l) ss
    | otherwise = stabilizedSeats ns
    where ns = step ss

main = do
    s <- getContents
    let ss = lines s

    print $ stabilizedSeats ss
