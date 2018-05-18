module Main where

import Language.Halon
import Data.Set

main :: IO ()
main = print (Argument Scalar Required (singleton Integer))
