module Main where  
  
remData :: [Int] -> Int -> Int -> [Int]  
remData [] _ _ = []  
remData (x:xs) a b  
  | x >= a && x <= b = x : remData xs a b  
  | otherwise        = remData xs a b  
  
orderDesc :: [Float] -> [Float]  
orderDesc [] = []  
orderDesc (x:xs) = ins x (orderDesc xs)  
  where  
    ins y [] = [y]  
    ins y (z:zs)  
      | y >= z    = y : z : zs  
      | otherwise = z : ins y zs  
  
main :: IO ()  
main = do  
  putStrLn "E3.1 remData"  
  print (remData [1,25,5,-4,7,0,5] 0 5)  
  putStrLn "E3.1 orderDesc"  
  print (orderDesc [1,25,5,-4,7,0,5])
