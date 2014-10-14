import Text.ParserCombinators.Parsec

import Language.Funct.Parser

main = do
    let programText = "a :: #ab5d42bfe\nb :: Int -> Int\nc = #67732abc2648fa\nd x y = x `div` y * 100\ne :: b -> String\ne f = show (f 42) ++ \"!\"\n"

    case parse functParser "Funct" programText of
        Left  error   -> print error
        Right program -> print program

