{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Main where

import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Language.Halon
import Language.Halon.SFD

example :: Function
example =
  Function { name = "example"
           , arguments =
               [ Argument { name = "x"
                          , displayName = Just "the value x"
                          , description = Nothing
                          , ty = ArgumentType Scalar Required
                                   (S.fromList [Integer, Boolean])
                          }
               , Argument { name = "y"
                          , displayName = Nothing
                          , description = Just "it's y"
                          , ty = ArgumentType Column Optional
                                   (S.fromList [DateTime, Boolean])
                          }
               ]
           , results =
               [ Result { name = "z"
                        , displayName = Nothing
                        , description = Nothing
                        , kind = Table
                        }
               ]
           , source = "function (x, y) { list(z = x + y) }"
           , description = Just "some function"
           , allowCaching = False
           }

main :: IO ()
main = do
  let sfd = toSFD example
  print sfd
  TIO.putStr $ render sfd
