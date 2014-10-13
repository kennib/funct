import Text.ParserCombinators.Parsec

import Language.Funct.Parser

main = do
    let programText = ""
    let programText1 = "int d34db33f  \nhello\nhello world\nhello|world|spaces\nhello :: int -> int\nhello = dog cat"
    let typeText1 = "person name age"
    let typeText2 = "(person) ((name)|(nickname)) (age)\n"
    let typeText3 = "person"
    let functionText1 = "int->int\ndouble pi"

    case parse typeParser "Funct" typeText2 of
        Left  error   -> print error
        Right program -> print program

    case functParse programText1 of
        Left  error   -> print error
        Right program -> print program
