module Axial(
	Axial()
	, makeAxial
	, axialDelta
	, axialToList
	, axialMaxValue
	, x
	, y
	, z
)
where

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