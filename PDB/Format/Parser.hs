{-# LANGUAGE
  NoMonomorphismRestriction
  #-}

module PDB.Format.Parser where

import PDB.Format.Types

import Text.ParserCombinators.Parsec

-- | Platform (Windows, *nix) agnostic line terminator
eol :: Parser String
eol =     try (string "\n\r")
      <|> try (string "\r\n")
      <|> string "\n"
      <|> string "\r"
      <?> "end of line"


-- | Join parsers. TODO: Implement as Arrow?
pjoin f p1 p2 = do
  v1 <- p1
  v2 <- p2
  return $ v1 `f` v2

(<++>) = pjoin (++)

positiveInt = many digit
negativeInt = option "" (string "-") <++> positiveInt

-- | Parsed (-/+) integers
integral :: (Integral i, Read i) => Parser i
integral = (negativeInt <|> positiveInt) >>= return . read





achar :: Parser AChar
achar =
    (letter <?> "achar: letter")
    >>= return . AChar

atom :: Parser Atom
atom =
    (many letter <?> "atom: letters")
    >>= return . AtomName

character :: Parser Character
character =
    (noneOf "\0'\a\b\t\n\f\v\r" <?> "character: no ascii control chars")
    >>= return . Character

continuation :: Parser Continuation
continuation =
    (try (count 2 (char ' ')) <|> (count 2 digit) <?> "continuation: 2 spaces or 2 digits")
    >>= return . Continuation

date :: Parser Date
date = undefined