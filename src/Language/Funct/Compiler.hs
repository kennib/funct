module Language.Funct.Compiler where

import Data.List (find)

import Language.Funct.AST
import Language.Funct.Linker

compile = compileHaskell . translateProgram

compileHaskell :: String -> String
compileHaskell = id

translateProgram :: Program -> String
translateProgram program = 
    (unlines $ map (translateType functions) types)
    ++ "\n"++
    (unlines $ map translateFunction functions)

    where Program typesHashed typesDefined functionsHashed functionsDefined = program
          types     = map getHashType     typesHashed     ++ typesDefined
          functions = map getHashFunction functionsHashed ++ functionsDefined

translateType :: [Definition (Function a)] -> Definition (Type String) -> String
translateType  functionDefs (Definition alias (Type s)) = case findAlias alias functionDefs of
    Just _  -> translateAlias alias ++ " :: " ++ s
    Nothing -> "type " ++ translateAlias alias ++ " = " ++ s

translateFunction :: Definition (Function String) -> String
translateFunction (Definition alias (Function s)) = translateAlias alias ++ " " ++ s

translateAlias :: Alias -> String
translateAlias (Alias s) = s

findAlias :: Alias -> [Definition a] -> Maybe (Definition a)
findAlias alias defs = find (\(Definition alias' _) -> alias == alias') defs
