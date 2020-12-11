import Text.Printf

oob :: (Int, Int) -> [String] -> Bool
oob (x, y) ss = (x < 0 || x >= xl) || (y < 0 || y >= yl)
    where xl = length $ head ss
          yl = length ss

neighborsIter :: Int -> (Int, Int) -> [String] -> Int
neighborsIter i (x0, y0) ss
    | i == 9                  = 0
    | i == 4 || oob (x, y) ss = 0 + neighborsIter (i+1) (x0, y0) ss
    | otherwise               = (fromEnum $ ((ss !! y) !! x) == '#') + neighborsIter (i+1) (x0, y0) ss
    where x = x0 + (i `mod` 3)
          y = y0 + (i `div` 3)
          xl = length $ head ss
          yl = length ss

occupiedNeighbors :: (Int, Int) -> [String] -> Int
occupiedNeighbors (x, y) = neighborsIter 0 (x-1, y-1)

occupiedVisibleIter :: (Int, Int) -> (Int, Int) -> [String] -> Int
occupiedVisibleIter (dx, dy) (x, y) ss
    | oob (x+dx, y+dy) ss = 0
    | c == 'L'            = 0
    | c == '#'            = 1
    | otherwise           = occupiedVisibleIter (dx, dy) (x+dx, y+dy) ss
    where c  = (ss !! (y+dy)) !! (x+dx)
          xl = length $ head ss
          yl = length ss

occupiedVisible :: (Int, Int) -> [String] -> Int
occupiedVisible (x, y) ss = sum $ map vis sls
    where vis = (\s -> occupiedVisibleIter s (x, y) ss)
          sls = [(-1, -1), (0, -1), (1, -1),
                 (-1,  0),          (1,  0),
                 (-1,  1), (0,  1), (1,  1)]

nextState :: Char -> Int -> Int -> Char
nextState c tol os
    | c == '.'  = '.'
    | os == 0   = '#'
    | os >= tol = 'L'
    | otherwise = c

stepIter :: ((Int, Int) -> [String] -> Int) -> Int -> (Int, Int) -> [String] -> [String] -> [String]
stepIter osf tol (x, y) ns ss
    | y == (length ss)        = init ns
    | x == (length $ head ss) = stepIter osf tol (0, y+1) (ns ++ [""]) ss
    | otherwise               = stepIter osf tol (x+1, y) (ls ++ [l ++ [n tol os]]) ss
    where os = osf (x,y) ss
          ls = init ns
          l  = last ns
          n  = nextState ((ss !! y) !! x)

step :: [String] -> [String]
step = stepIter occupiedNeighbors 4 (0, 0) [""]

stepVis :: [String] -> [String]
stepVis = stepIter occupiedVisible 5 (0, 0) [""]

stabilizedSeats :: ([String] -> [String]) -> [String] -> Int
stabilizedSeats sf ss
    | ns == ss  = sum $ map (\l -> length $ filter (=='#') l) ss
    | otherwise = stabilizedSeats sf ns
    where ns = sf ss

main = do
    s <- getContents
    let ss = lines s

    printf "Silver: %d\n" $ stabilizedSeats step ss
    printf "Gold:   %d\n" $ stabilizedSeats stepVis ss
