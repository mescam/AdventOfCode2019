digits :: Int -> [Int]
digits n
    | d > 0      = m:(digits d)
    | otherwise  = [m]
    where
        d = n `div` 10
        m = n `mod` 10

adjacentDouble :: [Int] -> Bool
adjacentDouble (x:[]) = False
adjacentDouble (x:y:tail) | x==y      = True
                          | otherwise = adjacentDouble (y:tail)

adjacentDoubleNotTriple :: [Int] -> Bool
adjacentDoubleNotTriple [] = False
adjacentDoubleNotTriple (x:[]) = False
adjacentDoubleNotTriple (x:y:[])  | x==y      = True
                                  | otherwise = False
adjacentDoubleNotTriple (x:y:z:tail) | x==y && y/=z = True
                                     | x==y && y==z = adjacentDoubleNotTriple (dropWhile (==x) tail)
                                     | otherwise    = adjacentDoubleNotTriple (y:z:tail)

ascending :: [Int] -> Bool
ascending (x:[]) = True
ascending (x:y:tail) | x<=y      = ascending (y:tail)
                     | otherwise = False

main :: IO()
main = do
    let range = map (reverse . digits) [197487..673251]
    -- solution to part 1
    print $ length [p | p <- range, adjacentDouble p, ascending p]
    -- solution to part 2
    print $ length [p | p <- range, (adjacentDoubleNotTriple p), ascending p]