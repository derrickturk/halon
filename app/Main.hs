module Main where

import Language.Halon
import Data.Set

main :: IO ()
main = print (ArgumentType Scalar Required (singleton Integer))
