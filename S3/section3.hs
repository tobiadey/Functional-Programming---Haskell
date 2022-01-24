module Section3 where
import qualified Data.Char as C

data Shape
	= Circle Double
	| Rectangle Double Double
	deriving (Eq, Show)

type Point = (Int,Int)

data Maybe a = Nothing | Just a
	deriving (Eq, Ord, Show)

data Either a b = Left a | Right b
	deriving (Eq, Ord, Show)

-- In terminal type: origin
origin :: Point
origin = (0,0)

-- In terminal type: testPoint
testPoint :: Point
testPoint = (2,5)

-- In terminal type: plusPoint origin testPoint
-- In terminal type: plusPoint (2,3) (5,5)
plusPoint :: Point -> Point -> Point
plusPoint (x1,y1) (x2,y2) = (x1+x2 , y1+y2)


-- Area of the shape
-- In terminal type: area (Circle 2)
-- In terminal type: area (Rectangle 2 4)
area :: Shape -> Double
area (Circle r) = pi*r*r
area (Rectangle w h) = w*h

-- Rotating a shape 90 degrees to the right
-- In terminal type: rotate (Circle 2)
-- In terminal type: rotate (Rectangle 2 4)
rotate :: Shape -> Shape
rotate (Circle r) = Circle r
rotate (Rectangle w h) = Rectangle h w


-- Scaling a shape by a given number
-- In terminal type: scale 2 (Circle 2)
-- In terminal type: scale 2 (Rectangle 2 4)
scale :: Double -> Shape -> Shape
scale x (Circle r) = Circle (r*x)
scale x (Rectangle w h) = Rectangle (w*x) (h*x)

charToNum :: Char -> Int
charToNum c
	| C.isDigit c = C.ord c - C.ord '0'
	| otherwise = 0

