import PicturesSVG 	

--1---------- Caballos:

cuatroImg :: Picture -> Picture

cuatroImg pic =
	left `beside` right
		where
		 left = pic `above` invertColour pic
		 right = invertColour  (flipV pic) `above` flipV pic




--2-----------Tablero de ajedrez:

blackWhite :: Integer -> Picture
blackWhite n
	| n <= 1 = black
	| otherwise = black `beside` whiteBlack (n-1)

whiteBlack :: Integer -> Picture
whiteBlack n
	| n <= 1 = white  
	| otherwise = white `beside` blackWhite (n-1)

blackChess :: Integer -> Integer -> Picture
blackChess n m
	| n <= 1 = blackWhite m
	| otherwise = blackWhite m `above` whiteChess (n-1) m

whiteChess :: Integer -> Integer -> Picture
whiteChess n m
	| n <= 1 = whiteBlack m
	| otherwise = whiteBlack m `above` blackChess (n-1) m

chess = whiteChess 8 8

--3---- Mezcla de figuras:

blackWhite' :: Integer -> Integer -> Picture
blackWhite' m n
	| m <=1 && m==(8-n+1) = black'
	| m <= 1 = black
	| m==(8-n+1)  = black' `beside` whiteBlack' (m-1) n
	| otherwise = black `beside` whiteBlack' (m-1) n

whiteBlack' :: Integer -> Integer -> Picture
whiteBlack' m n
	| m <=1 && m==n = white'
	| m <= 1 = white  
	| m==n = white' `beside` blackWhite' (m-1) n
	| otherwise = white `beside` blackWhite' (m-1) n

blackChess' :: Integer -> Integer -> Picture
blackChess' n m
	| n <= 1 = blackWhite' m n
	| otherwise = blackWhite' m n `above` whiteChess' (n-1) m

whiteChess' :: Integer -> Integer -> Picture
whiteChess' n m
	| n <= 1 = whiteBlack' m n
	| otherwise = whiteBlack' m n `above` blackChess' (n-1) m

chess' :: Picture
chess'= whiteChess' 8 8

white' = white `over` smallHorse 
black' =  invertColour (black `over` (flipV smallHorse)) 
	