module Main where

-- Módulo: Ítems 3.2 (series de aproximación) y 3.3 (DCT-II)
-- Descripción: Implementa aproximaciones de e^x, cos(x), ln(1+x) por series truncadas,
--              cálculo de error porcentual y la Transformada Discreta del Coseno (DCT-II).
-- Restricciones del enunciado: solo se usan operaciones fundamentales y cos embebido.
-- Evidencias de uso: ver función main (muestra aproximaciones, errores y DCT sobre un vector de ejemplo).

-- Nombre: myAbs
-- Propósito: Valor absoluto de un Double.
-- Entrada:  v :: Double
-- Salida:   |v| :: Double
-- Complejidad: O(1).
myAbs :: Double -> Double
myAbs v = if v < 0 then (-v) else v

-- Nombre: pow
-- Propósito: Potencia entera x^n con exponente n en Z (soporta n negativo).
-- Entradas:
--   x :: Double
--   n :: Int
-- Salida:
--   Double  -- x elevado a n; si n < 0, devuelve 1 / (x^|n|).
-- Precondiciones:
--   Si n < 0 entonces x /= 0 para evitar división por cero.
-- Complejidad: O(|n|) por recursión simple.
pow :: Double -> Int -> Double
pow x n = if n == 0 then 1 else if n > 0 then x * pow x (n-1) else 1 / pow x (-n)

-- Nombre: fact
-- Propósito: Factorial de n (n!), como Double.
-- Entrada:   n :: Int
-- Salida:    Double
-- Precondiciones: n >= 0 (para n <= 1 devuelve 1).
-- Complejidad: O(n).
-- Nota: Representado en Double; exacto para n pequeños.
fact :: Int -> Double
fact n = if n <= 1 then 1 else fromIntegral n * fact (n-1)

-- Nombre: expApprox
-- Propósito: Aproximar e^x con la serie de Taylor truncada a N términos.
-- Entradas:
--   x :: Double
--   n :: Int       -- número de términos (N >= 1)
-- Salida:
--   Double         -- aproximación de e^x
-- Precondiciones:
--   n >= 1.
-- Complejidad: O(n) más el costo de pow/fact por término.
expApprox :: Double -> Int -> Double
expApprox x n = go 0 0 where
  go acc k = if k >= n then acc else go (acc + term k) (k+1)
  term k = pow x k / fact k

-- Nombre: cosApprox
-- Propósito: Aproximar cos(x) con su serie de Taylor (solo términos pares) truncada a N términos.
-- Entradas:
--   x :: Double
--   n :: Int       -- número de términos (N >= 1)
-- Salida:
--   Double         -- aproximación de cos(x)
-- Precondiciones:
--   n >= 1.
-- Complejidad: O(n).
cosApprox :: Double -> Int -> Double
cosApprox x n = go 0 0 where
  go acc k = if k >= n then acc else go (acc + term k) (k+1)
  term k = let s = if mod k 2 == 0 then 1.0 else (-1.0)
               num = pow x (2*k)
               den = fact (2*k)
           in s * num / den

-- Nombre: ln1pApprox
-- Propósito: Aproximar ln(1 + x) con serie alternante truncada a N términos.
-- Entradas:
--   x :: Double    -- requiere |x| < 1 para convergencia
--   n :: Int       -- número de términos (N >= 1)
-- Salida:
--   Double         -- aproximación de ln(1+x)
-- Precondiciones:
--   |x| < 1 y n >= 1.
-- Complejidad: O(n).
ln1pApprox :: Double -> Int -> Double
ln1pApprox x n = go 0 1 where
  go acc k = if k > n then acc else
               let s = if mod k 2 == 1 then 1.0 else (-1.0)
               in go (acc + s * pow x k / fromIntegral k) (k+1)

-- Nombre: percentError
-- Propósito: Calcular el error porcentual entre una aproximación y un valor de referencia.
-- Entradas:
--   approx :: Double  -- valor aproximado
--   real   :: Double  -- valor de referencia
-- Salida:
--   Double            -- error en porcentaje (0..∞)
-- Detalles:
--   Usa un piso ε = 1e-12 para evitar división por cero cuando |real| ≈ 0:
--   error% = |approx - real| / max(|real|, ε) * 100
-- Complejidad: O(1).
percentError :: Double -> Double -> Double
percentError approx real =
  let d = if myAbs real < 1e-12 then 1e-12 else myAbs real
  in myAbs (approx - real) / d * 100

-- Nombre: errorExp
-- Propósito: Error porcentual de expApprox frente a exp embebida (referencia).
-- Entradas: x :: Double, n :: Int
-- Salida:   Double (error%).
errorExp :: Double -> Int -> Double
errorExp x n = percentError (expApprox x n) (exp x)

-- Nombre: errorCos
-- Propósito: Error porcentual de cosApprox frente a cos embebida (referencia).
-- Entradas: x :: Double, n :: Int
-- Salida:   Double (error%).
errorCos :: Double -> Int -> Double
errorCos x n = percentError (cosApprox x n) (cos x)

-- Nombre: errorLn1p
-- Propósito: Error porcentual de ln1pApprox frente a log(1+x) embebida (referencia).
-- Entradas: x :: Double, n :: Int
-- Salida:   Double (error%).
errorLn1p :: Double -> Int -> Double
errorLn1p x n = percentError (ln1pApprox x n) (log (1 + x))

-- Nombre: myLength
-- Propósito: Longitud de una lista (versión recursiva).
-- Entrada:   [a]
-- Salida:    Int
-- Complejidad: O(n).
myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

-- Nombre: sqrtApprox
-- Propósito: Aproximar sqrt(a) con método de Newton–Raphson.
-- Entrada:   a :: Double
-- Salida:    Double
-- Precondiciones:
--   a >= 0. Si a == 0, retorna 0; si a > 0, parte en a/2 y itera hasta 30 pasos o error < 1e-12.
-- Complejidad: O(iteraciones) con convergencia cuadrática típica.
sqrtApprox :: Double -> Double
sqrtApprox a = newton a (a/2) 0 where
  newton a x i = if i >= 30 || myAbs (x*x - a) < 1e-12 then x else newton a ((x + a / x) / 2) (i+1)

-- Nombre: aCoeff
-- Propósito: Coeficiente de normalización a(k) para la DCT-II.
-- Entradas:
--   k :: Int, n :: Int (tamaño de la señal)
-- Salida:
--   Double -- sqrt(1/n) si k==0; sqrt(2/n) si k>0 (usando sqrtApprox).
-- Precondiciones:
--   n >= 1.
aCoeff :: Int -> Int -> Double
aCoeff k n = if k == 0 then 1 / sqrtApprox (fromIntegral n) else sqrtApprox (2 / fromIntegral n)

-- Nombre: elemAt
-- Propósito: Obtener el elemento en la posición i (0-indexado) de una lista.
-- Entradas:
--   [a], i :: Int
-- Salida:
--   a
-- Precondiciones:
--   0 <= i < length lista (esta precondición la garantiza el uso interno en sumDCT).
elemAt :: [a] -> Int -> a
elemAt (y:_) 0 = y
elemAt (_:ys) i = elemAt ys (i-1)

-- Nombre: aPi
-- Propósito: Constante π con doble precisión.
-- Tipo: Double.
aPi :: Double
aPi = 3.141592653589793

-- Nombre: sumDCT
-- Propósito: Calcular la suma ∑ x(n) * cos(((n+0.5) * π * k) / N) para un k dado.
-- Entradas:
--   k  :: Int         -- índice de frecuencia
--   xs :: [Double]    -- señal de entrada
--   i  :: Int         -- índice actual de acumulación (uso interno)
-- Salida:
--   Double            -- suma parcial/total para ese k.
-- Precondiciones:
--   0 <= k <= N-1, donde N = length xs.
sumDCT :: Int -> [Double] -> Int -> Double
sumDCT k xs i =
  if i >= myLength xs then 0
  else
    let xn  = elemAt xs i
        arg = ((fromIntegral i + 0.5) * aPi * fromIntegral k) / fromIntegral (myLength xs)
    in xn * cos arg + sumDCT k xs (i+1)

-- Nombre: dct
-- Propósito: Calcular la DCT-II completa de la señal xs.
-- Entrada:   xs :: [Double]
-- Salida:    [Double]  -- vector X(k) para k = 0..N-1
-- Detalles:
--   Usa aCoeff y sumDCT; complejidad O(N^2).
dct :: [Double] -> [Double]
dct xs = go 0 where
  n = myLength xs
  go k = if k >= n then [] else let ak = aCoeff k n
                                    v  = ak * sumDCT k xs 0
                                in v : go (k+1)

-- Nombre: main
-- Propósito: Mostrar corridas de prueba de series (ítem 3.2) y DCT (ítem 3.3).
-- Entradas: ninguna.
-- Salida: imprime en consola aproximaciones, errores% y la DCT de un vector de ejemplo.
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
