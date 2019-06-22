module Main where

import Lib
import Data
import System.IO
import System.Random
import System.Console.ANSI

import Data.Char (toUpper)

main :: IO ()
main = do
    gen <- newStdGen
    let filledGrid = fillInBlanks gen grid
        game = newGame filledGrid hiddenWords
    hSetBuffering stdout NoBuffering
    playTurn game

resetScreen :: IO ()
resetScreen = clearScreen >> setSGR [Reset] >> setCursorPosition 0 0

playTurn :: Game -> IO ()
playTurn game = do
    resetScreen
    putStrLn . formatGame $ game
    putStrLn "Please enter a word> "
    word <- getLine
    let upperWord = map toUpper word
        newGame = playGame game upperWord
    if completed newGame then
        putStrLn "You won!"
    else
        playTurn newGame