
module Section2 where
-- import qualified Data.Char as C

-- The square of an integer
-- In terminal type: square 2
square :: Integer -> Integer
square n = n*n

-- In terminal type: norm 2 3
norm :: Double -> Double -> Double
norm x y = sqrt (x*x + y*y)

-- In terminal type: size
size :: Integer
size = 12+13

-- A function using relational operators
-- In terminal type: small 5
-- In terminal type: small 12
small :: Int -> Bool
small n = 0 <= n && n < 10


-- In terminal type: maxThree 2 10 5
maxThree :: Int -> Int -> Int -> Int
maxThree x y z 
 | x >= y && x >= z = x
 | y >= z = y
 | otherwise = z

-- In terminal type: maxim 10 5
maxim :: Int -> Int -> Int
maxim x y
 | x >= y = x
 | otherwise = y

-- The middle number of 3 integers after being arranged in order.
-- In terminal type: middleNumber 2 50 11
middleNumber :: Int -> Int -> Int -> Int
middleNumber x y z
 | between y x z = x
 | between x y z = y
 | otherwise = z


--HELPER FUNCTION
--between x y z is True if the value of y
--lies between the values of x and z
between :: Int -> Int -> Int -> Bool
between x y z = 
 (x <= y && y <= z) || (z <= y && y <= x)


--defines a colour type with 8 new values of that type
data Colour
	= Red | Green | Blue | Yellow
	| Cyan | Magenta | Black | White
	deriving (Show)

--Defining functions on enumerated types
-- In terminal type: invert Blue
-- In terminal type: invert Red
invert :: Colour -> Colour
invert c = case c of
	Red -> Cyan
	Green -> Magenta
	Blue -> Yellow
	Yellow -> Blue
	Cyan -> Red
	Magenta -> Green
	Black -> White
	White -> Black


