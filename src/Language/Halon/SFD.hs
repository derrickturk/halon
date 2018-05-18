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

import qualified Data.Text as T
import Data.Monoid ((<>))

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
