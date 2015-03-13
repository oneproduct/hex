import Data.Array

data Axial
	=	Axial
		{
			x :: Int
			, y :: Int
			, z ::Int
		}
	deriving
	(
		Show
		, Eq
	)

makeAxial :: Int -> Int -> Axial
makeAxial x y = Axial x y $ (-x) - y

axialDelta :: Axial -> Axial -> Axial
axialDelta (Axial x1 y1 z1) (Axial x2 y2 z2) = Axial (x1-x2) (y1-y2) (z1-z2)

axialToList :: Axial -> [Int]
axialToList a = [x a, y a, z a]

axialMaxValue :: Axial -> Int
axialMaxValue = maximum . map abs . axialToList

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

data HexGridIndex
	=	HexGridIndex
		{
			derp :: Array Int Int
		}

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


axialToOffset :: Axial -> (Int, Int)
axialToOffset (Axial x y z) = (x + (z - ((z `rem` 2)) `div` 2), z)