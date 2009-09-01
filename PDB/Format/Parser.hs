{-# LANGUAGE
  FlexibleInstances
  , NoMonomorphismRestriction
  , TypeSynonymInstances
  #-}

module PDB.Format.Parser where

import PDB.Format.Types

import Text.ParserCombinators.Parsec
import Data.Monoid

-- | Platform (Windows, *nix) agnostic line terminator
eol :: Parser String
eol =     try (string "\n\r")
      <|> try (string "\r\n")
      <|> string "\n"
      <|> string "\r"
      <?> "end of line"

instance Monoid (Parser String) where
    mempty = option "" (many anyChar)
    mappend p1 p2 = do
      p1' <- p1
      p2' <- p2
      return $ p1' ++ p2'

(<++>) :: (Monoid a) => a -> a -> a
l <++> r = mconcat [l,r]

positiveInt = many digit
negativeInt = option "" (string "-") <++> positiveInt

-- | Parse (-/+) integers
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