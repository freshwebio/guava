module Lexer (
  whiteSpace, lexer, reserved, identifier, comma, braces, parens,
  symbol, reservedOp
  ) where

import Control.Monad
import Text.Parsec
import Text.Parsec.Language
import qualified Text.Parsec.Token as Token

languageDef =
  emptyDef {
    Token.commentStart = "/*",
    Token.commentEnd = "*/",
    Token.commentLine = "//",
    Token.identStart = letter,
    Token.identLetter = alphaNum,
    Token.reservedNames = [
      "module", 
      "exposing", 
      "function", 
      "let", 
      "in", 
      "case", 
      "if", 
      "then", 
      "else",
      "type",
      "::",
      "->",
      "[]"
    ],
    Token.reservedOpNames = ["=", "not", "and", "or"],
    Token.caseSensitive = True
  }

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer

whiteSpace = Token.whiteSpace lexer

reserved = Token.reserved lexer

reservedOp = Token.reservedOp lexer

comma = Token.comma lexer

braces = Token.braces lexer

parens = Token.parens lexer

symbol = Token.symbol lexer