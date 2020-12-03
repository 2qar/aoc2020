treesHit :: (Int, Int) -> (Int, Int) -> [String] -> Int
treesHit (x, y) (dx, dy) m
        | y < length m = hit + treesHit (nx, y + dy) (dx, dy) m
        | otherwise    = 0
        where hit = fromEnum ((m !! y) !! x == '#')
              nx  = mod (x + dx) $ length (m !! 0)

slopes :: [(Int, Int)]
slopes = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]

solve :: [(Int, Int)] -> [String] -> Int
solve ss m = product $ map (\s -> treesHit (0, 0) s m) ss

main = interact $ show . solve slopes . lines