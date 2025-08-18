module Main where  

-- Módulo: Ítem 3.1 — Funciones útiles (remData, orderDesc)
-- Descripción: Implementa filtrado por intervalo y ordenamiento descendente.
-- Restricciones del enunciado: solo se usan operaciones fundamentales.
-- Evidencias de uso: ver función main (llamadas de prueba impresas por consola).

-- Nombre: remData
-- Propósito: Conservar únicamente los elementos de xs que pertenecen al intervalo cerrado [a, b].
-- Entradas:
--   xs :: [Int]  -- Lista de enteros a filtrar.
--   a  :: Int    -- Límite inferior del intervalo.
--   b  :: Int    -- Límite superior del intervalo.
-- Salida:
--   [Int]        -- Nueva lista con los elementos x de xs tales que a <= x <= b, en el mismo orden relativo.
-- Precondiciones:
--   a <= b (si no se cumple, el comportamiento no está definido por esta función).
-- Complejidad temporal: O(n), donde n = longitud de xs.
-- Notas:
--   - Lista vacía produce lista vacía.
--   - Se preserva el orden relativo (filtrado estable).
-- Ejemplo:
--   remData [1,25,5,-4,7,0,5] 0 5 == [1,5,0,5]
remData :: [Int] -> Int -> Int -> [Int]  
remData [] _ _ = []  
remData (x:xs) a b  
  | x >= a && x <= b = x : remData xs a b  
  | otherwise        = remData xs a b  

-- Nombre: orderDesc
-- Propósito: Ordenar una lista de Float en orden descendente (de mayor a menor) usando insertion sort recursivo.
-- Entrada:
--   [Float]      -- Lista a ordenar.
-- Salida:
--   [Float]      -- Misma lista ordenada en orden descendente.
-- Precondiciones:
--   Ninguna (soporta lista vacía).
-- Complejidad temporal:
--   Peor caso: O(n^2). Mejor caso (ya ordenada desc): O(n).
-- Notas:
--   - La inserción usa la comparación y >= z; en empates coloca el nuevo elemento antes del existente (no estrictamente estable).
--   - Funciona con negativos, ceros y repetidos.
-- Ejemplo:
--   orderDesc [1,25,5,-4,7,0,5] == [25,7,5,5,1,0,-4]
orderDesc :: [Float] -> [Float]  
orderDesc [] = []  
orderDesc (x:xs) = ins x (orderDesc xs)  
  where  
    ins y [] = [y]  
    ins y (z:zs)  
      | y >= z    = y : z : zs  
      | otherwise = z : ins y zs  

-- Nombre: main
-- Propósito: Mostrar corridas de prueba de las funciones del ítem 3.1.
-- Entradas: ninguna.
-- Salida: imprime en consola resultados de ejemplo.
main :: IO ()  
main = do  
  putStrLn "E3.1 remData"  
  print (remData [1,25,5,-4,7,0,5] 0 5)  
  putStrLn "E3.1 orderDesc"  
  print (orderDesc [1,25,5,-4,7,0,5])
