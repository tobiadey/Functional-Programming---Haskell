module Section5 where

import Data.Char
import Data.List


----------V1

--apply to all (without maps)
--turns each char in a list to ints
-- In terminal type: ordAll ['a','b','z','d']
ordAll :: [Char] -> [Int]
ordAll cs = [ord c | c <- cs]

--multiplies each int in a list by 2
-- In terminal type: doubleAll [2,3,4,5,6]
doubleAll :: [Int] -> [Int]
doubleAll ns = [2*n | n <- ns]



--selecting elements (filtering)
-- retuns the even numbers in a list
-- In terminal type: pickEven [1,2,3,4,4,4]
pickEven :: [Int] -> [Int]
pickEven ns = [n | n <- ns, even n]

--returns all letters in a list, ignores numbers
-- In terminal type: letters ['T','I','c','J','1','2','3']
letters :: [Char] -> [Char]
letters cs = [c | c <- cs, isAlpha c]



------ V2
--turns each char in a list to ints
-- In terminal type: ordAll ['a','b','z','d']
ordAll2 :: [Char] -> [Int]
ordAll2 cs = map ord cs

--multiplies each int in a list by 2
-- In terminal type: doubleAll2 [2,3,4,5,6]
doubleAll2 :: [Int] -> [Int]
doubleAll2 ns = map double ns
	where double n = 2*n

-- retuns the even numbers in a list
-- In terminal type: pickEven2 [1,2,3,4,4,4]
pickEven2 :: [Int] -> [Int]
pickEven2 ns = filter even ns

--returns all letters in a list, ignores numbers
-- In terminal type: letters2 ['T','I','c','J','1','2','3']
letters2 :: [Char] -> [Char]
letters2 cs = filter isAlpha cs




------V3

--multiplies each int in a list by 2
-- In terminal type: doubleAll3 [2..6]
doubleAll3 :: [Int] -> [Int]
doubleAll3 ns = map (\n -> 2*n) ns

--turns each char in a list to ints
-- In terminal type: ordAll3 ['a','b','z','d']
ordAll3 :: [Char] -> [Int]
ordAll3 = map ord

--returns all letters in a list, ignores numbers
-- In terminal type: letters3 ['T','I','c','J','1','2','3']
letters3 :: [Char] -> [Char]
letters3 = filter isAlpha


------V3
--we can rewrite the previous functions as

-- In terminal type: sum1 [1,2,3,4,5]
sum1 :: Num a => [a] -> a
sum1 xs = foldr (+) 0 xs

-- In terminal type: product1 [1,2,3,4,5]
product1 :: Num a => [a] -> a
product1 xs = foldr (*) 1 xs

-- In terminal type: and1 [True,False,True]
and1 :: [Bool] -> Bool
and1 xs = foldr (&&) True xs

-- In terminal type: or1 [True,False,True]
or1 :: [Bool] -> Bool
or1 xs = foldr (||) False xs

-- In terminal type: concat1 [[3,4,5],[2],[1,2,3]]
concat1 :: [[a]] -> [a]
concat1 xs = foldr (++) [] xs

-- In terminal type: maximum1 [1,2,3,4,5]
maximum1 :: Ord a => [a] -> a
maximum1 xs = foldr1 max xs

-- In terminal type: minimum1 [1,2,3,4,5]
minimum1 :: Ord a => [a] -> a
minimum1 xs = foldr1 min xs

