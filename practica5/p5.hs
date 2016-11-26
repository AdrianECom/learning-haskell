
ordenada :: Ord a => [a] -> Bool
ordenada [] = True
ordenada [x] = True
ordenada (x:y:xs) = (x < y) && ordenada(y:xs)

borrar :: Eq a => a -> [a] -> [a]
borrar x [] = []
borrar x (y:xs) = if x == y then xs else y:(borrar x xs)

insertar :: Ord a => a -> [a] -> [a]
insertar e [] = [e]
insertar e (x:xs) = if e <= x then e:(x:xs) else x:(insertar e xs)

ordInsercion :: Ord a => [a] -> [a]
ordInsercion [x] = [x]
ordInsercion (x:xs) = insertar x (ordInsercion (xs))

minimo :: Ord a => [a] -> a
minimo [x] = x
minimo xs = head (ordInsercion xs)

mezcla :: Ord a => [a] -> [a] -> [a]
mezcla [x] [y] = insertar x [y]
mezcla (x:xs) (y:ys) = insertar x (insertar y (mezcla xs ys))

mitades :: [a]-> ([a], [a])
mitades [] = []
mitades [x] = ([x],[])
mitades [x,y] = ([x],[y])
mitades xs = mitades()
