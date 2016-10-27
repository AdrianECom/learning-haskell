cuadrado:: Integer -> Integer
cuadrado x = x*x

cuarta:: Integer -> Integer
cuarta x = cuadrado(cuadrado x)

maximo:: Integer -> Integer -> Integer
maximo x y = if x >= y then x else y

area:: Integer -> Integer
area r = (22/7)*cuadrado(r)

fib:: Integer -> Integer
fib n = if n <= 1 then n else fib(n-1)+fib(n-2)

absol:: Integer -> Integer
absol x = if x < 0 then -x else x

aumentar:: Integer −>  Integer
aumentar x = cuadrado(x+1)

aumentar2:: Integer −>  Integer
aumentar2 x = cuadrado(x+1)

sucesor :: Integer -> Integer
sucesor = (+1)

aumentar3 :: Integer -> Integer
aumentar3 = cuadrado.sucesor

aumentar4 :: Integer -> Integer
aumentar4 = sucesor.cuadrado
