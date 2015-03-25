module Hex where

import Axial

data Hex
	=	Hex
		{
			pos :: Axial
		}
	deriving
	(
		Show
		, Eq
	)

data HexGrid
	=	HexGrid
		{
			shape :: HexShape
			, size :: Int
			, grid :: [Hex]
		}
	deriving
	(
		Show
		, Eq
	)

data HexShape
	= Hexagonal
	| Triangular
	| Rhomboidal
	deriving
	(
		Show
		, Eq
	)

type Size = Int
makeHexGrid :: HexShape -> Size -> HexGrid
makeHexGrid s n = HexGrid s n $ case s of
	Hexagonal -> [Hex (makeAxial x y) | x <- [-limit..limit], y <- [(max ((-limit) - x) (-limit))..(min limit (limit - x))]]
	Triangular -> [Hex (makeAxial x y) | x <- [0..limit], y <- [0..limit - x]]
	Rhomboidal -> [Hex (makeAxial x y) | x <- [0..limit], y <- [0..limit]]
	where limit = n - 1

getHexDistance :: Hex -> Hex -> Int
getHexDistance h1 h2 = maximum $ map abs [dx, dy, dz]
	where
		a = axialDelta (pos h1) (pos h2)
		dx = x a
		dy = y a
		dz = z a

areHexesAdjacent :: Hex -> Hex -> Bool
areHexesAdjacent a b = getHexDistance a b == 1

type Ring = Int
getHexesOnRing :: HexShape -> Ring -> Int
getHexesOnRing Hexagonal 1 = 1
getHexesOnRing s r = perimeter
	where
		sideLength = case s of
			Hexagonal -> r
			Triangular -> 3*r-1
			Rhomboidal -> 2*r
		numSides = case s of
			Hexagonal -> 6
			Triangular -> 3
			Rhomboidal -> 4
		sharedCorners = numSides
		perimeter = sideLength * numSides - sharedCorners
		
getHexesInHexShape :: HexShape -> Size -> Int
getHexesInHexShape s n = case s of
	Hexagonal -> 6 * sumOfFirstNumbers n + 1
	Triangular -> sumOfFirstNumbers n
	Rhomboidal -> n*n
	
sumOfFirstNumbers :: Int -> Int
sumOfFirstNumbers n = n*(n+1) `div` 2