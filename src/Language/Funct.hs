{-# LANGUAGE OverloadedStrings #-}

import Text.ParserCombinators.Parsec

import Language.Funct.Parser (functParser)
import Language.Funct.Compiler

import Network.Funct.Server

main = do
    functServer

    let programText = "A :: #ab5d42bfe\nB :: Int -> Int\nc = #67732abc2648fa\nd x y = x `div` y * 100\ne :: B -> String\ne f = show (f 42) ++ \"!\"\nmain = print $ e $ c 0"

    case parse functParser "Funct" programText of
        Left  error   -> print error
        Right program -> putStrLn $ compile program

