module Section10 where

import System.Random
import System.Directory

-- In terminal type: getInt (wehn promted with a blank line type in a number)
getInt :: IO Int
getInt = readLn

maxValue :: Int
maxValue = 100


--A program using getInt:
-- In terminal type: main1
main1 :: IO ()
main1 = do
	putStrLn "Enter two numbers"
	x <- getInt
	y <- getInt
	putStrLn ("The sum is " ++ show (x+y))

-- In terminal type: getSum
getSum :: IO Int
getSum = do
	x <- getInt
	y <- getInt
	return (x+y)

-- In terminal type: main
main :: IO ()
main = do
    n <- randomIO    -- from System.Random
    guessingGame (n `mod` maxValue + 1)

guessingGame :: Int -> IO ()
guessingGame target = do
    putStr $ "Guess a number between 1 and " ++ show maxValue ++ ": "
    guesses target 1

-- This recursive function gives the effect of a loop:
-- Here we have if expressions on IO actions and on strings.
-- In terminal type: guesses 19 5
guesses :: Int -> Int -> IO ()
guesses target nguesses = do
    guess <- getInt
    if guess == target then
        putStrLn $ "Correct in " ++ show nguesses ++ " guesses"
    else do
        putStr $ if guess > target
            then "Too high! " else "Too low! "
        putStr "Guess again: "
        guesses target (nguesses+1)

