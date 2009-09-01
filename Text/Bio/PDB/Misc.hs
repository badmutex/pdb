{-# LANGUAGE NoMonomorphismRestriction #-}

module Text.Bio.PDB.Misc where

import Chem.Chemistry

import Text.ParserCombinators.Parsec
import Control.Monad

-- | Platform (Windows, *nix) agnostic line terminator
eol :: GenParser Char st String
eol =     try (string "\n\r")
      <|> try (string "\r\n")
      <|> string "\n"
      <|> string "\r"
      <?> "end of line"

pjoin f p1 p2 = do
  v1 <- p1
  v2 <- p2
  return $ v1 `f` v2

(<++>) = pjoin (++)

positiveInt = many digit
negativeInt = option "" (string "-") <++> positiveInt

-- | Parsed (-/+) integers
integer :: GenParser Char st String
integer = negativeInt <|> positiveInt

-- | PDB serial numbers
serialNumber :: GenParser Char st String
serialNumber = integer

readM = return . read
readResidueM = return . readResidue