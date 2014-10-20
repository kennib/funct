module Language.Funct.Utils where

import Data.ByteString.Lazy.Char8 (pack)
import Data.Digest.Pure.SHA

import Language.Funct.AST

hashFunction :: Function String -> Hash
hashFunction (Function f) = Hash $ show $ sha1 $ pack f

