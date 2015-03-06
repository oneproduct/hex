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
	Hexagonal -> [Hex (makeAxial x y) | x <- [-limit..limit], y <- [(max (-limit) ((-limit) + x))..(min limit (limit - x))]]
	Triangular -> [Hex (makeAxial x y) | x <- [0..limit], y <- [0..limit - x]]
	Rhomboidal -> [Hex (makeAxial x y) | x <- [0..limit], y <- [0..limit]]
	where limit = n - 1

getHexDistance :: Hex -> Hex -> Int
getHexDistance h1 h2 = maximum $ map abs [dx, dy, dz]
	where
		a1 = pos h1
		a2 = pos h2
		x1 = x a1
		x2 = x a2
		dx = x1 - x2
		y1 = y a1
		y2 = y a2
		dy = y1 - y2
		z1 = z a1
		z2 = z a2
		dz = z1 - z2

areHexesAdjacent :: Hex -> Hex -> Bool
areHexesAdjacent a b = getHexDistance a b == 1
