module Language.Funct.Compiler where

import Control.Monad (mapM)
import Data.List (find)

import Language.Funct.AST
import Language.Funct.Linker

compile program = do
    program' <- translateProgram program
    return $ compileHaskell program'

compileHaskell :: String -> String
compileHaskell = id

translateProgram :: Program -> IO String
translateProgram program = do
    let types     = map getHashType     typesHashed     ++ typesDefined
    functionsHashDefined <- mapM getHashFunction functionsHashed
    let functions = functionsHashDefined ++ functionsDefined

    return $
        (unlines $ map (translateType functions) types)
        ++ "\n"++
        (unlines $ map translateFunction functions)

    where Program typesHashed typesDefined functionsHashed functionsDefined = program

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
