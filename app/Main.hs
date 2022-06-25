module Main where

import System.Console.ANSI
import System.IO

main :: IO ()
main = do
    setNoBuffering
    clearScreen
    putStrLn "Enter secret word to start: "
    hSetEcho stdin False -- disables echoing
    word <- getLine
    hSetEcho stdin True -- re-enables echoing
    clearScreen
    setCursorPosition 16 23
    putStrLn $ replicate (length word) '_'
    playGame word 7

setNoBuffering :: IO ()
setNoBuffering = do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering

-- combines setCursorPosition and putStr
setCPStr :: Int -> Int -> String -> IO ()
setCPStr x y z = do 
    setCursorPosition x y
    clearLine
    putStr z

-- initiates game
playGame :: String -> Int -> IO ()
playGame word chances = do
    hangman chances
    setCPStr 19 12 ("You have " ++ show chances ++ " more chance(s) left.")
    setCPStr 20 20 "Guess: "
    guess <- getLine
    case chances == 1 && guess /= word of 
        True -> do
            setCursorPosition 12 12
            clearScreen
            putStrLn $ "You lose! Word was \"" ++ word ++ ".\""
        False -> case guess == word of
            True -> do
                setCPStr 16 23 word
                setCPStr 19 12 "Winner winner, chicken dinner!\n"
            False -> do -- shows char/word if true
                setCPStr 16 23 (map (\x -> if elem x guess then x else '_') word)
                playGame word (chances - 1)

-- render post + body
hangman :: Int -> IO ()
hangman chances
    | chances == 7 = do
        setCPStr 9  25   "_____"
        setCPStr 10 25   "|   |"
        setCPStr 11 28      " |"
        setCPStr 12 28      " |"
        setCPStr 13 28      " |"
        setCPStr 14 23 "______|_"
    | chances == 6 = setCPStr 11 25   "O   |"
    | chances == 5 = setCPStr 12 24  "/    |"
    | chances == 4 = setCPStr 12 24  "/|   |"
    | chances == 3 = setCPStr 12 24  "/|\\  |"
    | chances == 2 = setCPStr 13 24  "/    |"
    | otherwise    = setCPStr 13 24  "/ \\  |"

