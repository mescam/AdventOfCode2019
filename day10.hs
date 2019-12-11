import Data.List

position :: Int -> Int -> (Int, Int)
position idx width = (idx `mod` width, idx `div` width)

inline :: (Int, Int) -> (Int, Int) -> (Int, Int) -> Bool
inline (x,y) (i,j) (q,p)
    | x==i = x==q 
    | otherwise = p*(x-i) == (q*(y-j) + (y*(x-i)) - (x*(y-j)))

findSteroids :: [Char] -> Int -> Int -> [(Int, Int)]
findSteroids [] p width = []
findSteroids (x:xs) p width | x=='#'    = (position p width):(findSteroids xs (p + 1) width)
                            | otherwise = findSteroids xs (p + 1) width

distance :: (Int, Int) -> (Int, Int) -> Int          
distance (x,y) (i,j) = ((i-x)*(i-x)) + ((j-y)*(j-y))

sameSide (x,y) (i,j) (a,b) | x==i && x==a = (j>y && b>y) || (j<y && b<y)
                           | otherwise = (i>x && a>x) || (i<x && a<x)


isObservable (x,y) (i,j) all = let
    line = filter (inline (x,y) (i,j)) all
    ss = filter (sameSide (x,y) (i,j)) line
    in (distance (x,y) (i,j)) == (minimum $ map (distance (x,y)) ss)

observables (x,y) all = let
    other = filter (/=(x,y)) all
    in filter (\a -> isObservable (x,y) a other) other

maximumBy2 :: (a -> a -> Bool) -> [a] -> a
maximumBy2 _ (x:[]) = x
maximumBy2 func (x:xs) | func x max = max
                       | otherwise  = x
                       where
                         max = maximumBy2 func xs

main :: IO()
main = do
    content <- readFile "day10_input.txt"
    let layers = lines content
    let width = (length $ (layers !! 0))
    let input = filter (/='\n') content
    -- print $ layers
    let steroids = findSteroids input 0 width
    -- print $ steroids 
    -- let s = inlineSteroids (2,2) steroids
    let almost = map (\x -> (x, length $ observables x steroids)) steroids
    -- print $ almost
    print $ maximumBy2 (\(x,y) (i,j) -> y < j) almost