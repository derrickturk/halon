{-# LANGUAGE OverloadedStrings #-}

module Language.Halon.SFD (
    Escaped
  , escape
  , getEscaped
  , Tag(..)
  , tag
  , tagWithAttrs
  , SFD(..)
  , ToSFD(..)
  , render
) where

import Language.Halon

import qualified Data.Set as S
import qualified Data.Text as T

import Data.Monoid ((<>))
import Data.Maybe (fromMaybe)

newtype Escaped = Escaped { getEscaped :: T.Text }
  deriving Show

escape :: T.Text -> Escaped
escape = Escaped
       . (T.replace "\"" "&quot;")
       . (T.replace "'" "&apos;")
       . (T.replace "<" "&lt;")
       . (T.replace ">" "&gt;")
       . (T.replace "&" "&amp;")

data Tag = 
  Tag { tagName :: T.Text
      , attrs :: [(T.Text, T.Text)]
      } deriving Show

tag :: T.Text -> Tag
tag name = Tag name []

tagWithAttrs :: T.Text -> [(T.Text, T.Text)] -> Tag
tagWithAttrs = Tag

data SFD =
    Text Tag Escaped
  | Container Tag [SFD]
  deriving Show

class ToSFD a where
  toSFD :: a -> SFD

instance ToSFD PrimitiveType where
  toSFD ty = Text (tag "AllowedDataType") (Escaped (sfdType ty)) where
    sfdType Integer = "Integer"
    sfdType Real = "Real"
    sfdType SingleReal = "SingleReal"
    sfdType Currency = "Currency"
    sfdType String = "String"
    sfdType Date = "Date"
    sfdType Time = "Time"
    sfdType DateTime = "DateTime"
    sfdType Boolean = "Boolean"
    sfdType Binary = "Binary"

instance ToSFD ValueKind where
  toSFD k = Text (tag "Type") (Escaped (sfdKind k)) where
    sfdKind Scalar = "Scalar"
    sfdKind Column = "Column"
    sfdKind Table = "Table"

instance ToSFD Argument where
  toSFD (Argument name displayName ty desc) = Container (tag "Input") $
    [ Text (tag "Name") (escape name)
    , Text (tag "DisplayName") (escape $ fromMaybe name displayName)
    , case desc of
        Nothing -> Container (tag "Description") []
        Just d -> Text (tag "Description") (escape d)
    ] ++ typeSFD ty
    where
      typeSFD (ArgumentType kind Required types) =
        [ toSFD kind ] ++ fmap toSFD (S.toList types)
      typeSFD (ArgumentType kind Optional types) =
        [ toSFD kind
        , Text (tag "IsOptional") (Escaped "true")
        ] ++ fmap toSFD (S.toList types)

indent :: Int -> T.Text
indent = flip T.replicate "  "

renderTagOpen :: Tag -> T.Text
renderTagOpen (Tag name attrs) = "<" <> name <> mconcat (renderAttr <$> attrs)
  where
    renderAttr (name, val) = " " <> name <> "=" <> "\"" <> val <> "\""

renderTagClose :: Tag -> T.Text
renderTagClose (Tag name _) = "</" <> name <> ">"

renderInner :: Int -> SFD -> T.Text
renderInner n (Text tag e) = indent n <> renderTagOpen tag <> ">"
  <> getEscaped e
  <> renderTagClose tag <> "\n"
renderInner n (Container tag []) = indent n <> renderTagOpen tag <> " />\n"
renderInner n (Container tag contents) = indent n <> renderTagOpen tag
  <> mconcat (renderInner (n + 1) <$> contents)
  <> indent n <> renderTagClose tag <> "\n"

render :: SFD -> T.Text
render = ("<?xml version=\"1.0\"?>" <>) . renderInner 0
