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
  toSFD (Argument name displayName desc ty) = Container (tag "Input") $
    [ Text (tag "Name") (escape name)
    , Text (tag "DisplayName") (escape $ fromMaybe name displayName)
    , case desc of
        Nothing -> Container (tag "Description") []
        Just d -> Text (tag "Description") (escape d)
    ] <> typeSFD ty
    where
      typeSFD (ArgumentType kind Required types) =
        [ toSFD kind ] <> fmap toSFD (S.toList types)
      typeSFD (ArgumentType kind Optional types) =
        [ toSFD kind
        , Text (tag "IsOptional") (Escaped "true")
        ] <> fmap toSFD (S.toList types)

maybeTextSFD :: Tag -> Maybe T.Text -> SFD
maybeTextSFD tag Nothing = Container tag []
maybeTextSFD tag (Just t) = Text tag (escape t)

instance ToSFD Result where
  toSFD (Result name displayName desc kind) = Container (tag "Output") $
    [ Text (tag "Name") (escape name)
    , Text (tag "DisplayName") (escape $ fromMaybe name displayName)
    , maybeTextSFD (tag "Description") desc
    , toSFD kind
    ]

-- TODO: not sure where the cache bit goes
instance ToSFD Function where
  toSFD (Function name args results src desc _) = Container
    (tagWithAttrs "ScriptFunctionDefinition"
      [ ("xmlns:xsd", "http://www.w3.org/2001/XMLSchema")
      , ("xmlns:xsi", "http://www.w3.org/2001/XMLSchema-instance")
      ]) $
    [ Container (tagWithAttrs "Version" [("Major", "1"), ("Minor", "0")]) []
    , Text (tag "Name") (escape name)
    , Text (tag "Script") (escape src)
    , Text (tag "Language") (Escaped "TERR")
    ]
    <> fmap toSFD args
    <> fmap toSFD results
    <> [ maybeTextSFD (tag "Description") desc ]

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
renderInner n (Container tag contents) = indent n <> renderTagOpen tag <> ">\n"
  <> mconcat (renderInner (n + 1) <$> contents)
  <> indent n <> renderTagClose tag <> "\n"

render :: SFD -> T.Text
render = ("<?xml version=\"1.0\"?>\n" <>) . renderInner 0
