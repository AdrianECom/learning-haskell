--1.-
cuadrado:: Integer -> Integer
cuadrado x = x * x

cuarta :: Integer -> Integer
cuarta x = cuadrado (cuadrado x)

cuarta2 :: Integer -> Integer
cuarta2 = cuadrado.cuadrado 

--2.-

maximo :: (Integer, Integer) -> Integer
maximo (x,y) = if x >= y then x else y

maximoc :: Integer -> Integer -> Integer
maximoc x y = if x >= y then x else y

--3.-

area :: Integer -> Float
area r = (22/7) *  fromInteger(cuadrado r)

cuadrado2:: Float -> Float
cuadrado2 x = x * x

area2 :: Float -> Float
area2 r = (22/7) *  cuadrado2 r

--4.-

fib :: Integer -> Integer
fib n = if n <= 1 then n else fib(n-1)+fib(n-2)

fib2 :: Integer -> Integer
fib2 0 = 0
fib2 1 = 1
fib2 n = fib2 (n-1)+fib2(n-2)

--5.-

--La funcion abs está definida en Prelude

absol :: Integer -> Integer
absol x = if x<0 then -x else x

--6.-

aumentar :: Integer -> Integer
aumentar x = cuadrado (x+1)

aumentar2 :: Integer -> Integer
aumentar2 x = 1 + cuadrado x

sucesor :: Integer -> Integer
sucesor = (+1)

aumentar' :: Integer -> Integer
aumentar' = cuadrado.sucesor

aumentar2' :: Integer -> Integer
aumentar2' = sucesor.cuadrado 

--7.-

nAnd :: Bool -> Bool -> Bool
nAnd True True = False
nAnd x y = True

nAnd' :: Bool -> Bool -> Bool
nAnd' x y
	| x && y = False
	| otherwise = True 
--8.- 
exOr, exOr', exOr'' :: Bool -> Bool -> Bool 

exOr True x = not x
exOr False x = x

exOr' x y = (x||y) && not(x&&y)

exOr'' x y = x/= y 

--9.-

minimo :: Integer -> Integer -> Integer
minimo x y = if x <= y then x else y

minimoTres :: Integer -> Integer -> Integer -> Integer
minimoTres x y z = minimo x (minimo y z)

--10.-

maxTres,maxTres' :: Integer -> Integer -> Integer -> Integer
maxTres x y z
	| x>=y && x>=z = x
	| y >=z = y
	| otherwise = z

maxTres' x y z = max (max x y) z

-- 'max' es una función predefinida. 

--11.-

--La función 'entre' devuelve 'True' si la segunda variable está entre la primera y la tercera.

entre :: Integer -> Integer -> Integer -> Bool
entre x y z = (x<=y && y<=z)|| (z<=y && y<=x)

numeroCentral :: Integer -> Integer -> Integer -> Integer
numeroCentral x y z
	| entre y x z = x
	| entre x y z = y
	| otherwise = z

numeroCentral' :: Integer -> Integer -> Integer -> Integer
numeroCentral' x y z = min (max x y) z

-- 'min' es una función predefinida. 

--12.-

productoRango :: Integer -> Integer -> Integer
productoRango m n 
	| m > n = 0
	| m == n = m
	| otherwise = m * productoRango (m+1) n

fact :: Integer -> Integer
fact 0 = 1
fact n = productoRango 1 n

--13.-

prod :: Int -> Int -> Int
prod x y 
	| x==0 || y==0 = 0
	| otherwise =  x + prod x (y-1)