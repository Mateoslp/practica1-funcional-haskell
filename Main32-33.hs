module Main where

myAbs :: Double -> Double
myAbs v = if v < 0 then (-v) else v

pow :: Double -> Int -> Double
pow x n = if n == 0 then 1 else if n > 0 then x * pow x (n-1) else 1 / pow x (-n)

fact :: Int -> Double
fact n = if n <= 1 then 1 else fromIntegral n * fact (n-1)

expApprox :: Double -> Int -> Double
expApprox x n = go 0 0 where
  go acc k = if k >= n then acc else go (acc + term k) (k+1)
  term k = pow x k / fact k

cosApprox :: Double -> Int -> Double
cosApprox x n = go 0 0 where
  go acc k = if k >= n then acc else go (acc + term k) (k+1)
  term k = let s = if mod k 2 == 0 then 1.0 else (-1.0)
               num = pow x (2*k)
               den = fact (2*k)
           in s * num / den

ln1pApprox :: Double -> Int -> Double
ln1pApprox x n = go 0 1 where
  go acc k = if k > n then acc else
               let s = if mod k 2 == 1 then 1.0 else (-1.0)
               in go (acc + s * pow x k / fromIntegral k) (k+1)

percentError :: Double -> Double -> Double
percentError approx real =
  let d = if myAbs real < 1e-12 then 1e-12 else myAbs real
  in myAbs (approx - real) / d * 100

errorExp :: Double -> Int -> Double
errorExp x n = percentError (expApprox x n) (exp x)

errorCos :: Double -> Int -> Double
errorCos x n = percentError (cosApprox x n) (cos x)

errorLn1p :: Double -> Int -> Double
errorLn1p x n = percentError (ln1pApprox x n) (log (1 + x))

myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

sqrtApprox :: Double -> Double
sqrtApprox a = newton a (a/2) 0 where
  newton a x i = if i >= 30 || myAbs (x*x - a) < 1e-12 then x else newton a ((x + a / x) / 2) (i+1)

aCoeff :: Int -> Int -> Double
aCoeff k n = if k == 0 then 1 / sqrtApprox (fromIntegral n) else sqrtApprox (2 / fromIntegral n)

elemAt :: [a] -> Int -> a
elemAt (y:_) 0 = y
elemAt (_:ys) i = elemAt ys (i-1)

aPi :: Double
aPi = 3.141592653589793

sumDCT :: Int -> [Double] -> Int -> Double
sumDCT k xs i =
  if i >= myLength xs then 0
  else
    let xn  = elemAt xs i
        arg = ((fromIntegral i + 0.5) * aPi * fromIntegral k) / fromIntegral (myLength xs)
    in xn * cos arg + sumDCT k xs (i+1)

dct :: [Double] -> [Double]
dct xs = go 0 where
  n = myLength xs
  go k = if k >= n then [] else let ak = aCoeff k n
                                    v  = ak * sumDCT k xs 0
                                in v : go (k+1)

main :: IO ()
main = do
  putStrLn "E3.2 expApprox, error%"
  print (expApprox 1.5 6, errorExp 1.5 6)
  putStrLn "E3.2 cosApprox, error%"
  print (cosApprox 1.5 6, errorCos 1.5 6)
  putStrLn "E3.2 ln1pApprox, error%"
  print (ln1pApprox 0.2 10, errorLn1p 0.2 10)
  putStrLn "E3.3 DCT"
  print (dct [1,2,3,4,5,6,7,8,9,10])
