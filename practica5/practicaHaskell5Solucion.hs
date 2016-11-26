import Test.QuickCheck

--1.- 
ordenada :: Ord a => [a] -> Bool
ordenada [] = True
ordenada [_] = True
ordenada (x:y:xs) = x <= y && ordenada (y:xs)

--2.- 
borrar :: Eq a => a -> [a] -> [a]
borrar x [] = []
borrar x (y:ys) | x == y = ys
	        | otherwise = y : borrar x ys

--3.- 
insertar :: Ord a => a -> [a] -> [a]
insertar e [] = [e]
insertar e (x:xs)
	| e<=x	= e:x:xs
	| otherwise = x:insertar e xs

--4.-
ordInsercion :: Ord a => [a] -> [a]
ordInsercion [] = []
ordInsercion (x:xs) = insertar x (ordInsercion xs)

ordInsercion2 :: Ord a => [a] -> [a]
ordInsercion2 = foldr insertar []

--5.-
minimo :: Ord a => [a] -> a
minimo = head . ordInsercion

--6.-
mezcla :: Ord a => [a] -> [a] -> [a]
mezcla [] ys = ys
mezcla xs [] = xs
mezcla (x:xs) (y:ys) | x <= y = x : mezcla xs (y:ys)
		     | otherwise = y : mezcla (x:xs) ys

mezcla2 :: Ord a => [a] -> [a] -> [a]
mezcla2 [] ys = ys
mezcla2 (x:xs) ys = insertar x (mezcla2 xs ys)

--7.-
mitades :: [a] -> ([a],[a])
mitades xs = splitAt (length xs `div` 2) xs

--8.-
ordMezcla :: Ord a => [a] -> [a]
ordMezcla [] = []
ordMezcla [x] = [x]
ordMezcla xs = mezcla (ordMezcla ys) (ordMezcla zs)
			where (ys,zs) = mitades xs

ordMezcla2 :: Ord a => [a] -> [a]
ordMezcla2 [] = []
ordMezcla2 [x] = [x]
ordMezcla2 xs = mezcla (ordMezcla2 ys) (ordMezcla2 zs)
			where ys = take mitad xs
	     		      zs = drop mitad xs
	    	              mitad = (length xs) `div` 2 
--9.- 
esPermutacion :: Eq a => [a] -> [a] -> Bool
esPermutacion [] [] = True
esPermutacion [] (y:ys) = False
esPermutacion (x:xs) ys = elem x ys && esPermutacion xs (borrar x ys)

--10.- La propiedad es
prop_ordMezcla_ordenada :: Ord a => [a] -> Bool
prop_ordMezcla_ordenada xs = ordenada (ordMezcla xs)

--La comprobación es
--ghci> quickCheck prop_ordMezcla_ordenada
-- +++ OK, passed 100 tests.

--11.- La propiedad es
prop_ordMezcla_permutacion :: Ord a => [a] -> Bool
prop_ordMezcla_permutacion xs = esPermutacion (ordMezcla xs) xs

--La comprobación es
--ghci> quickCheck prop_ordMezcla_permutacion
-- +++ OK, passed 100 tests.
