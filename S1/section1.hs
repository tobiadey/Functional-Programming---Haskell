module Section1 where

-- A constant integer.
-- In terminal type: size
size :: Integer
size = 12+13

-- The square of an integer.
-- In terminal type: square 2
square :: Integer -> Integer
square n = n*n


-- Triple an integer.
-- In terminal type: triple 2
triple :: Integer -> Integer
triple n = 3*n

-- Triple an integer and returns the square
-- In terminal type: squareOfTriple 2
squareOfTriple :: Integer -> Integer
squareOfTriple n = square (triple n)