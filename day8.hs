import Data.List
intoLayers _ [] = []
intoLayers len xs = (take len xs):(intoLayers len $ drop (len) xs)

countOccurrences :: [Int] -> (Int, Int, Int) -> (Int, Int, Int)
countOccurrences [] (zeros, ones, twos) = (zeros, ones, twos)
countOccurrences (x:xs) (zeros, ones, twos)
    | x == 0 = countOccurrences xs (zeros+1, ones, twos)
    | x == 1 = countOccurrences xs (zeros, ones+1, twos)
    | x == 2 = countOccurrences xs (zeros, ones, twos+1)
    | otherwise = countOccurrences xs (zeros, ones, twos)

minimumBy2 :: (a -> a -> Bool) -> [a] -> a
minimumBy2 _ (x:[]) = x
minimumBy2 func (x:xs) | func x min = min
                       | otherwise  = x
                       where
                         min = minimumBy2 func xs

findPixel xs = head $ dropWhile (==2) xs

maybeNewLine pos width | ((pos) `mod` width) == 0 = "\n"
                       | otherwise = ""

getImage [] _ _ = ""
getImage (x:xs) pos width | x==0 = " " ++ (maybeNewLine pos width) ++ (getImage xs (pos+1) width)
                          | otherwise = "X" ++ (maybeNewLine pos width) ++ (getImage xs (pos+1) width)

main :: IO()
main = do
    content <- readFile "day8_input.txt"

    let digits = map (\x -> read x ::Int) (map (:[]) content)
    let width = 25
    let height = 6
    
    let layerLength = width * height
    let layers = intoLayers layerLength digits

    let occurr = map (\x -> countOccurrences x (0, 0, 0)) layers
    print $ minimumBy2 (\(x,_,_) -> \(y,_,_) -> x > y) occurr

    -- let image = map (findPixel) (transpose [[0,2,2,2], [1,1,2,2], [2,2,1,2], [0,0,0,0]])
    let image = map (findPixel) (transpose layers)

    putStr $ getImage image 1 width
