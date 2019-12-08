split :: String -> [String]
split [] = [""]
split (c:cs) | c == ','  = "" : rest
             | otherwise = (c : head rest) : tail rest
    where rest = split cs

parse :: String -> (String, Int)
parse str = (take 1 str, (read (drop 1 str)::Int))

addPoints :: (Int, Int, Int, Int) -> [(Int, Int)] -> [(Int, Int)]
addPoints (0, 0, 0, 0) acc = acc
addPoints (l, u, d, r) acc
    | l > 0 = addPoints (l-1, u, d, r) ((lx-1, ly):acc)
    | u > 0 = addPoints (l, u-1, d, r) ((lx, ly+1):acc)
    | d > 0 = addPoints (l, u, d-1, r) ((lx, ly-1):acc)
    | r > 0 = addPoints (l, u, d, r-1) ((lx+1, ly):acc)
    | otherwise = acc
    where
        (lx, ly) = head acc

points :: [(String, Int)] -> [(Int, Int)] -> [(Int, Int)]
points [] acc = acc
points ((op,len):xs) acc
    | op == "L" = points xs (addPoints (len, 0, 0, 0) acc)
    | op == "U" = points xs (addPoints (0, len, 0, 0) acc)
    | op == "D" = points xs (addPoints (0, 0, len, 0) acc)
    | op == "R" = points xs (addPoints (0, 0, 0, len) acc)

intersect [] _ acc = acc
intersect (x:xs) ys acc | elem x ys       = intersect xs ys (x:acc)
                        | otherwise       = intersect xs ys acc

distance (x, y) = (abs x) + (abs y)

main :: IO()
main = do
    content <- readFile "day3_input.txt"
    let vectors = lines content
    let v1 = map parse $ split (vectors !! 0)
    let v2 = map parse $ split (vectors !! 1)

    let pv1 = points v1 [(0, 0)]
    let pv2 = points v2 [(0, 0)]


    print $ minimum (map (\(x, _) -> x) (filter (\(x, _) -> x /= 0) (map (\x -> (distance x, x)) (intersect pv1 pv2 []))))
    -- Possible optimization: generate distances for each point of 2 vectors, sort, find first common element - that's the answer