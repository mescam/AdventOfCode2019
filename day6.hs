import Data.List
import Data.Maybe

parse :: String -> (String, String)
parse str = let
    before = takeWhile (/=')') str
    after = drop ((length before)+1) str
    in (before, after)

swap (a, b) = (b, a)

all2 [] = []
all2 ((a,b):xs) = a:b:(all2 xs)

unique [] = []
unique (x:xs) | elem x back = back
              | otherwise   = x:back
              where
                back = unique xs

findOrbit :: String -> [(String, String)] -> Maybe String
findOrbit x [] = Nothing
findOrbit x ((b, a):ys) | x==b = Just a
                        | otherwise = findOrbit x ys

findSteps x y backtrack | found==y = 1
                        | otherwise = 1 + (findSteps found y backtrack)
                        where
                            found = fromJust $ findOrbit x backtrack

findPath x y backtrack | found==y = []
                       | otherwise = found:(findPath found y backtrack)
                       where
                        found = fromJust $ findOrbit x backtrack

findFirstCommonElement :: [String] -> [String] -> Maybe String
findFirstCommonElement [] _ = Nothing
findFirstCommonElement (x:xs) ys | elem x ys = Just x
                                 | otherwise = findFirstCommonElement xs ys

main :: IO()
main = do
    content <- readFile "day6_input.txt"
    let xs = lines content
    let pairs = map parse xs
    let backtrack = map swap pairs

    let planets = unique $ all2 $ map swap pairs

    print $ sum $ map (\x -> findSteps x "COM" backtrack) $ filter (/="COM") planets

    let you = findPath "YOU" "COM" backtrack
    let san = findPath "SAN" "COM" backtrack

    let common = fromJust $ findFirstCommonElement you san
    print $ (findSteps "YOU" common backtrack) + (findSteps "SAN" common backtrack) - 2


