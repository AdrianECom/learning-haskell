---1.

type Racional1 = (Int,Int)

simplificar1 (n,d) = (((signum d)*n) `div` m, (abs d) `div`m)
	where m = gcd n d

r1Mul :: Racional1 -> Racional1 -> Racional1
r1Mul (n1,d1) (n2,d2) = simplificar1 (n1*n2,d1*d2)

r1Div :: Racional1 -> Racional1 -> Racional1
r1Div (n1,d1) (n2,d2) = simplificar1 (n1*d2,d1*n2) 

r1Sum :: Racional1 -> Racional1 -> Racional1
r1Sum (n1,d1) (n2,d2) = simplificar1 (n1*d2+n2*d1,d1*d2) 

r1Res :: Racional1 -> Racional1 -> Racional1
r1Res (n1,d1) (n2,d2) = simplificar1 (n1*d2-n2*d1,d1*d2) 

muestraRacional1 :: Racional1 -> String
muestraRacional1 (n,d)
	| d' == 1 = show n'
	| otherwise = show n' ++ "/" ++ show d'
		where (n',d') = simplificar1 (n,d)

---2.

data Racional2 = Rac Integer Integer
	deriving (Eq)

simplificar2 :: Racional2 -> Racional2
simplificar2 (Rac n d) = (Rac (((signum d)*n) `div` m)  ((abs d) `div`m))
	where m = gcd n d

r2Mul :: Racional2 -> Racional2 -> Racional2
r2Mul (Rac n1 d1) (Rac n2 d2) = simplificar2 (Rac (n1*n2) (d1*d2))

r2Div :: Racional2 -> Racional2 -> Racional2
r2Div (Rac n1 d1) (Rac n2 d2) = simplificar2 (Rac (n1*d2) (d1*n2)) 

r2Sum :: Racional2 -> Racional2 -> Racional2
r2Sum (Rac n1 d1) (Rac n2 d2) = simplificar2 (Rac (n1*d2+n2*d1) (d1*d2)) 

r2Res :: Racional2 -> Racional2 -> Racional2
r2Res (Rac n1 d1) (Rac n2 d2) = simplificar2 (Rac (n1*d2-n2*d1) (d1*d2)) 

instance Show Racional2 where
	show (Rac n 1) = show n 
	show (Rac n d ) = show n' ++ "/" ++ show d'
		where (Rac n' d') = simplificar2 (Rac n d)
	
---3.

instance Num Racional2 where
	(*) = r2Mul
	(+) = r2Sum 
	(-) = r2Res 
	negate (Rac n1 d1)	= simplificar2 (Rac (-n1) d1)
	fromInteger x =  (Rac x 1)
	signum (Rac n d) = if (n*d)<0 then -1 else 1 
	abs (Rac n d)= if (n*d)<0 then simplificar2(Rac (-n) d) else simplificar2(Rac n d)

