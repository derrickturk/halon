{-# LANGUAGE DuplicateRecordFields #-}

module Language.Halon (
    PrimitiveType(..)
  , ValueKind(..)
  , Optionality(..)
  , ArgumentType(..)
  , Argument(..)
  , Function(..)
) where

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
           , displayName :: Maybe T.Text
           , ty :: ArgumentType
           , description :: Maybe T.Text
           } deriving Show

data Result =
  Result { name :: T.Text
         , displayName :: Maybe T.Text
         , description :: Maybe T.Text
         , kind :: ValueKind
         } deriving Show

data Function =
  Function { arguments :: [(T.Text, ArgumentType)]
           , result :: Result
           , source :: T.Text
           , description :: Maybe T.Text
           , allowCaching :: Bool
           } deriving Show
