module Section4 where
import qualified Data.Char as C
import Data.List


-- In terminal type: value
value :: String
value = "abcde"

-- reverses a list
-- In terminal type: replicate1 10 5
replicate1 :: Int -> a -> [a]   
replicate1 n x = take n (repeat x)

-- [x | (n, x) <- zip [1..] "abcde", odd n]
-- [x | (n, x) <- zip [1..] "abcde", even n]

-- find inxdexes with matching values from two strings
-- In terminal type: indexMatch "ok" "op"
indexMatch ::String -> String-> [Char]
indexMatch w1 w2 = [x | (x, y) <- zip w1 w2, x == y]

-- elements in odd-numbered positions, counting from 1
-- In terminal type: odds [1,2,3,4,5]
odds :: [a] -> [a]
odds xs = [x | (n, x) <- zip [1..] xs, odd n]

-- elements in even-numbered positions, counting from 1
-- In terminal type: evens [1,2,3,4,5]
evens :: [a] -> [a]
evens xs = [x | (n, x) <- zip [1..] xs, even n]



