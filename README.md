# Práctica 1 — Programación Funcional (Haskell)

**Curso:** Lenguajes y Paradigmas de Programación\
**Tema:** Programación Funcional

S2566-0528-0529\
**Inicio:** 5 de agosto de 2025\
**Entrega:** 24 de agosto de 2025 (23:59)\
**Valor:** 12% de la nota del curso

Este README documenta el desarrollo de los ítems 3.1, 3.2 y 3.3 usando Haskell. Incluye explicación de uso, decisiones de diseño y cómo reproducir las pruebas.

---

## 0. Integrantes y enlaces

- **Integrantes:** Juan Fernando Gomez Rivas, Ikker Mateo Gil Jordán.
- **Repositorio GitHub:** [https://github.com/Mateoslp/practica1-funcional-haskell](https://github.com/Mateoslp/practica1-funcional-haskell)
- \*\*Video en YouTube: \*\* 

En el video deben presentarse los integrantes (con cámara), explicar el código, señalar problemas/soluciones y mostrar una ejecución de pruebas.

---

## 1. Objetivos

1. Aplicar conceptos básicos de la programación funcional.
2. Desarrollar aplicaciones simples en Haskell.
3. Presentar el desarrollo de forma clara (código, reporte, video y evidencias).

---

## 2. Reglas y restricciones de la práctica

- **Funciones permitidas** (fundamentales): `+`, `-`, `*`, `/`, `mod`, `toEnum`, `fromEnum`, `fromIntegral`, `cos`.
- **Cualquier otra función** utilizada debe implementarse manualmente y quedar en el código (por ejemplo: `pow`, `fact`, `sqrtApprox`, utilidades recursivas, etc.).
- **Comentarios de cabecera:** cada función creada debe indicar qué hace, entradas y salidas.
- **Repositorio con reporte:** este README actúa como reporte.
- **Video:** todos los integrantes deben hablar y aparecer.

Nota: Para calcular **errores porcentuales**, se comparan las aproximaciones con funciones embebidas de Haskell (por ejemplo, `exp`, `cos`, `log`) tal como solicita el enunciado. El uso de `cos` está explícitamente permitido; `exp` y `log` se usan solo como **referencia** para el valor “real” en el cálculo del error.

---

## 3. Cómo compilar y ejecutar

### Opción A: GHC directo

```bash
# Ítem 3.1
ghc -O2 -Wall -Wextra -o e31 src/Main31.hs
./e31

# Ítems 3.2 y 3.3
ghc -O2 -Wall -Wextra -o e3233 src/Main3233.hs
./e3233
```

### Opción B: GHCi (interactivo)

```bash
ghci src/Main31.hs
:main

-- o cargar funciones y probar manualmente
:reload
remData [1,25,5,-4,7,0,5] 0 5
orderDesc [1,25,5,-4,7,0,5]
```

Se recomienda compilar con `-O2` para la DCT si se prueban listas largas.

---

## 4. Ítem 3.1 — Funciones útiles

### 4.1 `remData :: [Int] -> Int -> Int -> [Int]`

- **Descripción:** filtra los elementos de la lista que quedan dentro del intervalo cerrado `[a, b]`.
- **Entradas:** lista de `Int`, límite inferior `a`, límite superior `b`.
- **Salida:** nueva lista con los elementos `x` tales que `a ≤ x ≤ b`.
- **Idea de implementación:** recursión por patrones (`[]` y `(x:xs)`) más guardas para decidir si se conserva `x`.
- **Ejemplo de uso:**
  ```haskell
  remData [1,25,5,-4,7,0,5] 0 5   -- => [1,5,0,5]
  ```
  Se preserva el orden relativo de los elementos válidos.

### 4.2 `orderDesc :: [Float] -> [Float]`

- **Descripción:** ordena una lista en **orden descendente**.
- **Entradas:** lista de `Float`.
- **Salida:** lista ordenada de mayor a menor.
- **Idea de implementación:** insertion sort recursivo: se ordena la cola y se inserta la cabeza en la posición correcta mediante `ins`.
- **Ejemplo de uso:**
  ```haskell
  orderDesc [1,25,5,-4,7,0,5]     -- => [25,7,5,5,1,0,-4]
  ```
- **Consideraciones:** `Float` puede acumular error de redondeo; si se desea estabilidad total sobre empates, puede ajustarse la comparación.

---

## 5. Ítem 3.2 — Métodos numéricos por series

Se implementan aproximaciones por series truncadas con `N` términos:

### 5.1 Exponencial `expApprox :: Double -> Int -> Double`

$$
\mathrm{e}^x \approx \sum_{k=0}^{N-1} \frac{x^k}{k!}
$$

- **Entradas:** `x` (valor), `N` (términos).
- **Salida:** aproximación de `e^x`.
- **Soporte:** `pow` (potencia entera), `fact` (factorial).
- **Ejemplo de uso:** `expApprox 1.5 6`

### 5.2 Coseno `cosApprox :: Double -> Int -> Double`

$$
\cos(x) \approx \sum_{k=0}^{N-1} (-1)^k \frac{x^{2k}}{(2k)!}
$$

- **Entradas:** `x`, `N`.
- **Salida:** aproximación de `cos(x)`.
- **Ejemplo:** `cosApprox 1.5 6`

### 5.3 Logaritmo natural en 1+x `ln1pApprox :: Double -> Int -> Double`

$$
\ln(1+x) \approx \sum_{k=1}^{N} (-1)^{k+1} \frac{x^k}{k}, \quad |x|<1
$$

- **Entradas:** `x` con **restricción** `|x| < 1`, `N`.
- **Salida:** aproximación de `ln(1+x)`.
- **Ejemplo:** `ln1pApprox 0.2 10`
- **Convergencia:** para `|x| ≥ 1` la serie no converge; usar otro método.

### 5.4 Cálculo de error porcentual

`percentError :: Double -> Double -> Double` reporta:

error% = |aprox − real| / max(|real|, ε) × 100, con ε = 1e−12.

Funciones de conveniencia:\
`errorExp x n` compara `expApprox x n` vs `exp x`.\
`errorCos x n` compara `cosApprox x n` vs `cos x`.\
`errorLn1p x n` compara `ln1pApprox x n` vs `log (1+x)`.

### 5.5 Utilidades adicionales

- `pow x n`: potencia entera (maneja `n` negativo invirtiendo la base).
- `fact n`: factorial (con `Double` para productos grandes; exacto para `n` pequeños).
- `myAbs v`: valor absoluto.

---

## 6. Ítem 3.3 — Transformada Discreta del Coseno (DCT)

Se implementa la **DCT-II** con coeficiente de normalización:

$$
X(k) = a(k) \sum_{n=0}^{N-1} x(n) \cdot \cos\Big(\frac{(n+0.5)\,\pi\,k}{N}\Big),\quad 0\le k\le N-1
$$

con

$$
 a(k) = \begin{cases}
 \sqrt{\tfrac{1}{N}}, & k = 0 \\
 \sqrt{\tfrac{2}{N}}, & k > 0
 \end{cases}
$$

### 6.1 Funciones principales

- `dct :: [Double] -> [Double]`\
  Devuelve la lista `[X(0), X(1), ..., X(N-1)]`.
- `sumDCT k xs i`\
  Acumula la suma para un `k` dado recorriendo `xs` desde el índice `i`.
- `aCoeff k n`\
  Calcula `a(k)` usando una aproximación de raíz (ver abajo) para cumplir la restricción de no usar `sqrt` de la librería.

### 6.2 Raíz cuadrada sin librerías

- `sqrtApprox :: Double -> Double`\
  Implementada vía Newton–Raphson, itera hasta 30 pasos o hasta error < `1e-12`.

### 6.3 Detalles de implementación

- No se usa indexación nativa; se proporciona `elemAt` y `myLength` para operar sobre listas recursivamente.
- `π` se define como `aPi = 3.141592653589793`.

### 6.4 Ejemplo de uso

```haskell
dct [1,2,3,4,5,6,7,8,9,10]
-- Resultado esperado cercano al enunciado:
-- [17.3925, -9.0249, 0, -0.9667, 0, -0.3162, 0, -0.1279, 0, -0.0359]
```

Pueden existir pequeñas diferencias por redondeo o por la aproximación de `sqrt`.

---

## 7. Resultados de las pruebas para cada ejercicio solicitado

Nota: Corridas simuladas con los mismos algoritmos del código. Valores redondeados a 6–8 cifras.

### 7.1 Ítem 3.1 — `remData`, `orderDesc`

\*\*A) \*\*``

| Caso | Entrada lista       | a | b | Salida          |
| ---- | ------------------- | - | - | --------------- |
| 1    | `[1,25,5,-4,7,0,5]` | 0 | 5 | `[1,5,0,5]`     |
| 2    | `[]`                | 0 | 5 | `[]`            |
| 3    | `[-10,-5,-1]`       | 0 | 5 | `[]`            |
| 4    | `[0,1,2,3,4,5]`     | 0 | 5 | `[0,1,2,3,4,5]` |
| 5    | `[6,7,8]`           | 0 | 5 | `[]`            |
| 6    | `[3,3,3]`           | 3 | 3 | `[3,3,3]`       |

\*\*B) \*\*``

| Caso | Entrada               | Salida                |
| ---- | --------------------- | --------------------- |
| 1    | `[1,25,5,-4,7,0,5]`   | `[25,7,5,5,1,0,-4]`   |
| 2    | `[3.2,3.2,1.1]`       | `[3.2,3.2,1.1]`       |
| 3    | `[]`                  | `[]`                  |
| 4    | `[-1.5,-3.0,0.0,2.2]` | `[2.2,0.0,-1.5,-3.0]` |

---

### 7.2 Ítem 3.2 — Series y errores porcentuales

\*\*A) Aproximación de e^x con \*\*``

| x    | N | Aproximación | Valor real (`exp x`) | Error %      |
| ---- | - | ------------ | -------------------- | ------------ |
| 1.5  | 2 | 2.50000000   | 4.48168907           | 44.21745996  |
| 1.5  | 4 | 4.18750000   | 4.48168907           | 6.56424544   |
| 1.5  | 6 | 4.46171875   | 4.48168907           | 0.44559808   |
| 1.5  | 8 | 4.48092913   | 4.48168907           | 0.01695657   |
| -1.0 | 2 | 0.00000000   | 0.36787944           | 100.00000000 |
| -1.0 | 4 | 0.33333333   | 0.36787944           | 9.39060572   |
| -1.0 | 6 | 0.36666667   | 0.36787944           | 0.32966629   |
| -1.0 | 8 | 0.36785714   | 0.36787944           | 0.00606131   |

\*\*B) Aproximación de cos x con \*\*``

| x   | N | Aproximación | Valor real (`cos x`) | Error %      |
| --- | - | ------------ | -------------------- | ------------ |
| 1.5 | 2 | -0.12500000  | 0.07073720           | 276.71041129 |
| 1.5 | 4 | 0.07011719   | 0.07073720           | 0.87650367   |
| 1.5 | 6 | 0.07073693   | 0.07073720           | 0.00037823   |
| 1.5 | 8 | 0.07073720   | 0.07073720           | 0.00000004   |
| 3.0 | 2 | -3.50000000  | -0.98999250          | 253.53803307 |
| 3.0 | 4 | -1.13750000  | -0.98999250          | 14.89986075  |
| 3.0 | 6 | -0.99104911  | -0.98999250          | 0.10672915   |
| 3.0 | 8 | -0.98999449  | -0.98999250          | 0.00020185   |

**C) Aproximación de ln(1+x) con **``** (válido si |x| < 1)**

| x   | N  | Aproximación | Valor real (`log(1+x)`) | Error %     |
| --- | -- | ------------ | ----------------------- | ----------- |
| 0.2 | 2  | 0.18000000   | 0.18232156              | 1.27333094  |
| 0.2 | 4  | 0.18226667   | 0.18232156              | 0.03010622  |
| 0.2 | 6  | 0.18232000   | 0.18232156              | 0.00085387  |
| 0.2 | 8  | 0.18232151   | 0.18232156              | 0.00002645  |
| 0.2 | 10 | 0.18232156   | 0.18232156              | 0.00000086  |
| 0.8 | 2  | 0.48000000   | 0.58778666              | 18.33771866 |
| 0.8 | 4  | 0.54826667   | 0.58778666              | 6.72352753  |
| 0.8 | 6  | 0.57011200   | 0.58778666              | 3.00698637  |
| 0.8 | 8  | 0.57909979   | 0.58778666              | 1.47789515  |
| 0.8 | 12 | 0.58535787   | 0.58778666              | 0.41321107  |

---

### 7.3 Ítem 3.3 — DCT

**A) Señal del enunciado**\
Entrada: `[1,2,3,4,5,6,7,8,9,10]`\
Salida DCT (aprox.): `[17.3925, -9.0249, 0.0000, -0.9667, 0.0000, -0.3162, 0.0000, -0.1279, 0.0000, -0.0359]`

**B) Señal constante**\
Entrada: `[5,5,5,5,5,5,5,5]`\
Salida DCT (aprox.): `[14.1421, ~0, ~0, ~0, ~0, ~0, ~0, ~0]`\
Solo `X(0)` es significativo (energía concentrada en la componente DC).

**C) Rampa**\
Entrada: `[1,2,3,4,5,6,7,8]`\
Salida DCT (aprox.): `[12.7279, -6.4423, ~0, -0.6735, 0.0000, -0.2009, ~0, -0.0507]`

**D) Impulso**\
Entrada: `[1,0,0,0,0,0,0,0]`\
Salida DCT (aprox.): `[0.3536, 0.4904, 0.4619, 0.4157, 0.3536, 0.2778, 0.1913, 0.0975]`

---

## 8. Problemas encontrados y cómo los solucionamos

- **Dominio de la serie de ln(1+x).**\
  Problema: con `x` cercanos o mayores a 1, la serie no converge o converge muy lento.\
  Solución: documentar la restricción `|x| < 1` y usar valores válidos (por ejemplo, `x = 0.2`, `0.8`).

- **Errores porcentuales cuando el valor real ≈ 0.**\
  Problema: si `real` es casi cero (por ejemplo, `cos` cerca de π/2), el denominador hace explotar el error%.\
  Solución: definir `percentError` con piso numérico `ε = 1e-12` en el denominador: `max(|real|, ε)`.

- **Crecimiento de factorial/potencias para **``** grandes.**\
  Problema: `fact n` y `pow x n` crecen rápidamente y pueden causar pérdidas de precisión o tiempos altos.\
  Solución: limitar `N` en pruebas (p. ej., hasta 12) y considerar recurrencias término a término como mejora futura.

- **Diferencia numérica leve en la DCT vs. el ejemplo.**\
  Problema: diferencias en la cuarta o quinta cifra decimal frente al enunciado.\
  Solución: usar `aPi = 3.141592653589793` (doble precisión) y `sqrtApprox` por Newton con tolerancia `1e-12`.

- **Rendimiento O(N^2) de la DCT.**\
  Problema: para listas largas, `dct` con doble bucle puede ser lenta.\
  Solución: aceptable para la práctica; como mejora opcional, precalcular cosenos y usar acumuladores estrictos o `foldl'`.

- **Ambigüedad de tipos en GHCi al probar **``**.**\
  Problema: al pasar literales enteros a una función tipada con `[Float]`, GHCi puede mostrar *defaulting warnings*.\
  Solución: probar con literales con punto decimal (`[1.0,25.0,5.0]`) o anotar `:: [Float]`.

- **Seguridad de índices en **``**.**\
  Problema: `elemAt` falla si se pide un índice fuera del rango.\
  Solución: el flujo de `sumDCT` controla los límites con `myLength`, por lo que no se generan índices inválidos (precondición documentada).

---

## 9. Conclusiones

- Las aproximaciones por series mejoran al aumentar `N`, pero el costo computacional exige balancear precisión/tiempo.
- La serie de `ln(1+x)` muestra buena precisión para `|x|<1`; fuera de ese rango no es apropiada.
- La DCT concentra energía en bajas frecuencias para señales suaves (constante/rampa), lo que explica su uso en compresión.
- Implementar utilidades (potencia, factorial, raíz por Newton) refuerza recursión, patrones y diseño funcional.

---

## 10. Apéndice — Referencias rápidas de uso

```haskell
-- 3.1
remData [1,25,5,-4,7,0,5] 0 5    -- [1,5,0,5]
orderDesc [1,25,5,-4,7,0,5]      -- [25,7,5,5,1,0,-4]

-- 3.2
expApprox 1.5 6                  -- aprox e^1.5 usando 6 términos
cosApprox 1.5 6                  -- aprox cos(1.5) usando 6 términos
ln1pApprox 0.2 10                -- aprox ln(1+0.2) usando 10 términos
errorExp 1.5 6                   -- error% vs exp embebida
errorCos 1.5 6                   -- error% vs cos embebida
errorLn1p 0.2 10                 -- error% vs log(1+x) embebida

-- 3.3
dct [1,2,3,4,5,6,7,8,9,10]
```

---

## 11. Transparencia sobre el uso de IA

Usamos IA para dos tareas concretas y declaradas: (1) proponer la estructura inicial de este README (secciones y orden) y (2) apoyar la corrección de errores en las últimas versiones del código.

### Errores detectados y correcciones asistidas por IA

1. Off-by-one en la DCT (argumento del coseno).\
   Síntoma: amplitudes anómalas en el espectro.\
   Causa: se evaluaba `cos(π k n / N)` en vez de `cos(π k (n + 0.5) / N)`.\
   Aporte de IA: cotejar con la fórmula del enunciado y añadir el `+ 0.5`.\
   Acción aplicada: corregimos la expresión a `((n + 0.5) * π * k) / N`.

2. Inestabilidad de `sqrtApprox` cuando `a = 0`.\
   Síntoma: división por cero/`NaN` en la primera iteración de Newton.\
   Aporte de IA: agregar guardas: si `a == 0` retornar `0` y rechazar `a < 0`.\
   Acción aplicada: añadimos casos base y documentamos la precondición `a ≥ 0`.

3. Error porcentual inestable cuando el valor real ≈ 0.\
   Síntoma: error% excesivo cerca de ceros de `cos`.\
   Aporte de IA: usar un piso `ε = 1e-12` en el denominador (`max(|real|, ε)`).\
   Acción aplicada: incorporado en `percentError` y documentado.

4. Comparador en `orderDesc` (orden incorrecto en una versión previa).\
   Síntoma: resultados ascendentes en ciertos casos.\
   Aporte de IA: invertir guarda a `y >= z` y probar con negativos/repetidos.\
   Acción aplicada: corregido y añadido caso de prueba.

5. Pérdida de precisión en series con `N` grande.\
   Síntoma: desviaciones en `expApprox`/`ln1pApprox`.\
   Aporte de IA: acotar `N` en pruebas, usar `Double` (ya aplicado) y considerar recurrencias término a término como mejora.\
   Acción aplicada: acotamos `N` y lo dejamos en documentación.

6. Warnings de tipado en GHCi (defaulting).\
   Síntoma: literales enteros en funciones de `[Float]/[Double]`.\
   Aporte de IA: anotar tipos y usar literales con punto decimal en ejemplos.\
   Acción aplicada: actualizado en demostraciones del README.

