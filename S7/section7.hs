module Section7 where
import Data.Char
import Data.List


-- In terminal type: null1 []
-- In terminal type: null1 [1]
--null2 & null3 give the same result
null1 :: [a] -> Bool
null1 [] = True
null1 (x:xs) = False

null2 :: [a] -> Bool
null2 [] = True
null2 xs = False

null3 :: [a] -> Bool
null3 [] = True
null3 _ = False

--retrun the head of the list
-- In terminal type: head1 [1,2,3]
head1 :: [a] -> a
head1 (x:xs) = x

--remove the head of the list
-- In terminal type: tail1 [1,2,3]
tail1 :: [a] -> [a]
tail1 (x:xs) = xs

--get the maximum number out of 3 inputs
-- In terminal type: maxOfThree 1 50 2
--maxOfThree2 give the same result

maxOfThree :: Int -> Int -> Int -> Int
maxOfThree x y z = max (max x y) z

maxOfThree2 :: Int -> Int -> Int -> Int
maxOfThree2 x y z = maximum [x,y,z]

-- In terminal type: zip1 [1,3,5] [22,33,44]
zip1 :: [a] -> [b] -> [(a,b)]
zip1 (x:xs) (y:ys) = (x,y):zip1 xs ys
zip1 _ _ = []

-- Testing whether a value is in a list:
-- In terminal type: elem1 2 [3,4,5,2]
elem1 :: Eq a => a -> [a] -> Bool
elem1 x [] = False
elem1 x (y:ys)
	| x == y = True
	| otherwise = elem1 x ys


-- In terminal type: take1 5 [1,3,66,7,9,5,2,1,9]
take1 :: Int -> [a] -> [a]
take1 n [] = []
take1 n (x:xs)
	| n > 0 = x:take1 (n-1) xs
	| otherwise = []

-- Generalizing selecting elements from a list that are letters
-- In terminal type: letters1 ['r','4','f']
letters1 :: [Char] -> [Char]
letters1 [] = []
letters1 (c:cs)
	| isAlpha c = c : letters1 cs
	| otherwise = letters1 cs

-- to take a function returning a Bool as a parameter:
-- In terminal type: filter1 (<5)[1,3,5,7,9]
filter1 :: (a -> Bool) -> [a] -> [a]
filter1 p [] = []
filter1 p (x:xs)
	| p x = x : filter1 p xs
	| otherwise = filter1 p xs


--Getting letters from the start of a list:
-- In terminal type: takeLetters1 ['r','e','f']
takeLetters1 :: [Char] -> [Char]
takeLetters1 [] = []
takeLetters1 (c:cs)
	| isAlpha c = c : takeLetters1 cs
	| otherwise = []

-- Generalizing over the predicate:
-- In terminal type: takeWhile1 (<2)[1,2,2,3,3,4]
takeWhile1 :: (a -> Bool) -> [a] -> [a]
takeWhile1 p [] = []
takeWhile1 p (x:xs)
	| p x = x : takeWhile1 p xs
	| otherwise = []

-- Getting the rest
-- In terminal type: dropLetters1 ['r','e','f','4']
dropLetters1 :: [Char] -> [Char]
dropLetters1 [] = []
dropLetters1 (c:cs)
	| isAlpha c = dropLetters1 cs
	| otherwise = c:cs


-- In terminal type: dropWhile1 (<2)[1,2,2,3,3,4]
dropWhile1 :: (a -> Bool) -> [a] -> [a]
dropWhile1 p [] = []
dropWhile1 p (x:xs)
	| p x = dropWhile1 p xs
	| otherwise = x:xs



-- why we add 1 to most of these fuctions?
-- its because these fuction names are already exisiting in the prelude 
-- therefore it is ambigious and we need to define which one we want to call
-- so we can change the name to 
-- e.g takeWhile1


