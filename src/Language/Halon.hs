{-# LANGUAGE DuplicateRecordFields #-}

module Language.Halon where

import qualified Data.Set as S
import qualified Data.Text as T

data PrimitiveType =
    Integer
  | Real
  | SingleReal
  | Currency
  | String
  | Date
  | Time
  | DateTime
  | Boolean
  | Binary
  deriving (Eq, Show)

data ValueKind =
    Scalar
  | Column
  | Table
  deriving (Eq, Show)

data Optionality =
    Required
  | Optional
  deriving (Eq, Show)

data ArgumentType = ArgumentType ValueKind Optionality (S.Set PrimitiveType)
  deriving (Eq, Show)

data Argument =
  Argument { name :: T.Text
           , ty :: ArgumentType
           , description :: Maybe T.Text
           } deriving Show

data Function =
  Function { arguments :: [(T.Text, ArgumentType)]
           , allowCaching :: Bool
           , source :: T.Text
           , description :: Maybe T.Text
           , resultKind :: ValueKind
           } deriving Show
