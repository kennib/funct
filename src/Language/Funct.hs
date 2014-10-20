{-# LANGUAGE OverloadedStrings #-}

import Text.ParserCombinators.Parsec

import Language.Funct.Parser (functParser)
import Language.Funct.Compiler

main = do
    --let programText = "A :: #ab5d42bfe\nB :: Int -> Int\nc = #67732abc2648fa\nd x y = x `div` y * 100\ne :: B -> String\ne f = show (f 42) ++ \"!\"\nmain = print $ e $ c 0"
    let programText = "a = #87ea5dfc8b8e384d848979496e706390b497e547\nmain = print $ a 10"

    case parse functParser "Funct" programText of
        Left  error   -> print error
        Right program -> do
            prog <- compile program
            putStrLn prog

