module Simple
where

-- Definieren Sie eine Funktion fib zur Berechung der Fibonacci-Zahlen
-- ab 0 
fib     :: Integer -> Integer
fib x
    | x <= 1 = x
    | otherwise = fib (x-1) + fib (x-2)


-- Definieren Sie eine Funktion fib zur Berechung der Fibonacci-Zahlen
-- ab 0 mit linearer Laufzeit

fib2    :: Integer -> Integer
fib2 = fib2' 0 1 
       where 
       fib2' x0 x1 0 = x0
       fib2' x0 x1 n = fib2' x1 (x0 + x1) (n-1) 

-- Definieren Sie eine Funktion c (für Collatz), die berechnet
-- wie viele Rekursionsschritte benötigt werden, um
-- eine natürliche Zahl n >= 1 auf 1 zu
-- reduzieren.
--
-- Folgende Reduktionsregel sind dabei anzuwenden: Wenn n gerade ist,
-- so wird n halbiert, wenn n ungerade ist, so wird n verdreifacht und um
-- 1 erhöht.

c       :: Integer -> Integer
c n
  | n < 1 = error "Illegal argument"
  | n == 1 = 0
  | even n = 1 + c (n `div` 2)
  | otherwise = 1 + c (3*n+1)


-- Definieren Sie ein endrekurive Variante von c

c1      :: Integer -> Integer
c1 n
  | n < 1 = error "Illegal argument"
  | otherwise = c1' n 0
  where
    c1'   :: Integer -> Integer -> Integer
    c1' n counter
      | n == 1 = counter
      | even n = c1' (n `div` 2) (counter+1)
      | otherwise = c1' (3*n+1) (counter+1)


-- Definieren Sie eine Funktion cmax, die für ein
-- Intervall von Zahlen das Maximum der
-- Collatz-Funktion berechnet. Nutzen Sie die
-- vordefinierten Funkt min und max.

cmax    :: Integer -> Integer -> Integer
cmax lb ub
  | lb > ub = error "Illegal arguments"
  | lb < 0 = error "Illegal arguments"
  | lb == ub = c lb
  | otherwise = max (c lb) (cmax (lb+1) ub)


-- Definieren Sie eine Funktion imax, die für ein
-- Intervall von Zahlen das Maximum einer
-- ganzzahligen Funktion berechnet. Formulieren
-- Sie die obige Funktion cmax so um, dass sie mit imax arbeitet.

imax    :: (Integer -> Integer) -> Integer -> Integer -> Integer
imax f lb ub
  | lb > ub = error "Illegal arguments"
  | lb < 0 = error "Illegal arguments"
  | lb == ub = f lb
  | otherwise = max (f lb) (imax f (lb+1) ub)


cmax1   :: Integer -> Integer -> Integer
cmax1
    = imax c1

-- Entwickeln Sie eine Funktion,
-- die die Position und den Wert bestimmt, an der
-- das Maximum angenommen wird.
-- Versuchen Sie, eine endrekursive Lösung zu finden
-- (mit einer lokalen Hilfsfunktion).

imax2   :: (Integer -> Integer) -> Integer -> Integer -> (Integer, Integer)
imax2 f lb ub
  | lb > ub = error "Illegal arguments"
  | lb < 0 = error "Illegal arguments"
  | lb == ub = (lb, f lb)
  | otherwise = imax2' f lb ub (0, 0)
  where
    imax2'  :: (Integer -> Integer) -> Integer -> Integer -> (Integer, Integer) -> (Integer, Integer)
    imax2' f lb ub (pIndex, pMax)
      | lb > ub = (pIndex, pMax)
      | newValue > pMax = imax2' f (lb+1) ub (lb, newValue)
      | otherwise = imax2' f (lb+1) ub (pIndex, pMax)
        where
        newValue = f(lb)
-- ----------------------------------------
