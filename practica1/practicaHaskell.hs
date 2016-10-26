cuadrado:: Float -> Float
cuadrado x = x*x

cuarta:: Float -> Float
cuarta x = cuadrado(cuadrado x)

maximo:: Float -> Float -> Float
maximo x y = if x >= y then x else y

area:: Float -> Float
area r = (22/7)*cuadrado(r)

fib :: Float -> Float
fib n = if n <= 1 then n else fib(n-1)+fib(n-2)
