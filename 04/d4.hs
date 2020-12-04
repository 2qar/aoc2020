import Data.List
import Text.Printf

contains :: String -> String -> Bool
contains sub s
        | (length sub) > (length s) = False
        | otherwise                 = (isPrefixOf sub s) || (contains sub $ tail s)

valid1 :: String -> Bool
valid1 s = foldl (&&) True [contains f s | f <- fs]
        where fs = ["byr:", "iyr:", "eyr:", "hgt:", "hcl:", "ecl:", "pid:"]

validByr :: Int -> Bool
validByr y = (length $ show y) == 4 && y >= 1920 && y <= 2002

validIyr :: Int -> Bool
validIyr y = (length $ show y) == 4 && y >= 2010 && y <= 2020

validEyr :: Int -> Bool
validEyr y = (length $ show y) == 4 && y >= 2020 && y <= 2030

validHgt :: String -> Bool
validHgt s
    | (contains "cm" s) = n >= 150 && n <= 193
    | (contains "in" s) = n >= 59 && n <= 76
    | otherwise         = False
    where n = read $ takeWhile (\c -> c `elem` ['0'..'9']) s

validHcl :: String -> Bool
validHcl s = (head s) == '#' && (length [c | c <- s, c `elem` ['0'..'9'] || c `elem` ['a'..'f']]) == 6

validEcl :: String -> Bool
validEcl s = s `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

validPid :: String -> Bool
validPid s = (length [c | c <- s, c `elem` ['0'..'9']]) == 9

validFields :: [String] -> Bool
validFields fs
        | null fs    = True -- bad
        | f == "byr" = (validByr $ read v) && validFields n
        | f == "iyr" = (validIyr $ read v) && validFields n
        | f == "eyr" = (validEyr $ read v) && validFields n
        | f == "hgt" = (validHgt v) && validFields n
        | f == "hcl" = (validHcl v) && validFields n
        | f == "ecl" = (validEcl v) && validFields n
        | f == "pid" = (validPid v) && validFields n
        | f == "cid" = validFields n
        | otherwise  = False
        where f = takeWhile (\c -> c /= ':') (head fs)
              v = drop 1 $ dropWhile (\c -> c /= ':') (head fs)
              n = tail fs

valid2 :: String -> Bool
valid2 s = (valid1 s) && (validFields $ words s)

solve :: (String -> Bool) -> [String] -> Int
solve valid s
    | null s    = 0
    | otherwise = (fromEnum $ valid c) + solve valid n
    where c = unwords $ takeWhile (\l -> not (null l)) s
          n = drop 1 $ dropWhile (\l -> not (null l)) s

main = do 
    s <- getContents
    let ps = lines s

    printf "Silver: %d\n" $ solve valid1 ps
    printf "Gold:   %d\n" $ solve valid2 ps
