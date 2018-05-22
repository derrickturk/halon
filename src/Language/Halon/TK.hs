{-# LANGUAGE OverloadedStrings #-}

module Language.Halon.TK (
    Parser
) where

import Data.Void
import qualified Data.Text as T

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Language.Halon

data TK =
    TKFnName T.Text
  | TKNoCache
  | TKDescription T.Text
  | TKInput Argument
  | TKOutput Result

type Parser = Parsec Void T.Text

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

symbol :: T.Text -> Parser T.Text
symbol = L.symbol space

enclosed :: T.Text -> T.Text -> Parser a -> Parser a
enclosed left right = between (symbol left) (symbol right)

fnName :: Parser TK
fnName = char '#' *> space *> "TK_FN" *> space1 *>
  ((\c cs -> TKFnName (T.pack $ c:cs)) <$> letterChar <*> many alphaNumChar)

noCache :: Parser TK
noCache = char '#' *> space *> "TK_NOCACHE" *> pure TKNoCache
