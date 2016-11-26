import PicturesSVG


-- Ejercicio 1
a,b,c,d :: Picture -> Picture
a x = x
b x = invertColour(x `beside` (flipV x))
c x = invertColour(x)
d x = (flipV x) `beside` x

compose :: Picture -> Picture

compose x = (a(x) `beside` b(x)) `above` (c(x) `beside` d(x))

renderHorse = render( compose(horse) )





-- Ejercicio 2

chess :: (Integer -> Integer) -> Picture
chess n m =
  if n == 1 then
    white
  else
    (a(x) `beside` b(x)) `above` (c(x) `beside` d(x))




renderChess = render(chess(3,3))
