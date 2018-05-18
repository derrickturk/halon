module Language.Halon where

import qualified Data.Set as S

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

data ArgumentType = Argument ValueKind Optionality (S.Set PrimitiveType)
  deriving (Eq, Show)
