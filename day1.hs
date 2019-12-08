rocketFuelBasic m = max (quot m 3 - 2) 0 

rocketFuelAcc 0 acc = acc
rocketFuelAcc m acc = 
  let df = rocketFuelBasic m
  in rocketFuelAcc df (acc + df)
rocketFuel m = rocketFuelAcc m 0

main :: IO()
main = do
  content <- readFile "day1_input.txt"
  let masses = map  (read::String->Int) (lines content)
  print $ sum (map rocketFuel masses)
