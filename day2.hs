split :: String -> [String]
split [] = [""]
split (c:cs) | c == ','  = "" : rest
             | otherwise = (c : head rest) : tail rest
    where rest = split cs

merge [] ys = ys
merge (x:xs) ys = x:merge xs ys

rewriteList :: [Int] -> Int -> Int -> [Int]
rewriteList xs pos x = let
    before = take pos xs
    after = drop (pos + 1) xs
    in merge before (x:after)

execute :: [Int] -> Int -> [Int] -> [Int]
execute codes op args 
    | op == 1       = rewriteList codes out (val1+val2)
    | op == 2       = rewriteList codes out (val1*val2)
    where
        val1 = codes !! (args !! 0)
        val2 = codes !! (args !! 1)
        out = args !! 2

decode :: [Int] -> Int -> Int
decode codes cur
    | op == 99      = codes !! 0
    | otherwise     = decode (execute codes op args) (cur + 4)
    where 
        op = codes !! cur
        args = take 3 $ drop (cur+1) codes

findSolution :: [(Int, Int)] -> [Int] -> Int
findSolution ((noun, verb):pairs) codes
    | result==19690720      = 100*noun+verb
    | otherwise             = findSolution pairs codes
    where
        replaced = rewriteList (rewriteList codes 1 noun) 2 verb
        result = decode replaced 0

findSolutionInit :: [Int] -> Int
findSolutionInit codes = let 
    args = [0..99]
    pairs = [(noun,verb) | noun <- args, verb <- args]
    in findSolution pairs codes

main :: IO()
main = do
  content <- readFile "day2_input.txt"
  let code = map (read::String->Int) (split content)
  --print $ decode (rewriteList (rewriteList code 1 12) 2 2) 0
  --print $ decode [1,9,10,3,2,3,11,0,99,30,40,50] 0
  print $ findSolutionInit code