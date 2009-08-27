module Text.Bio.PDB.Misc where

import Text.ParserCombinators.Parsec

-- | Platform (Windows, *nix) agnostic line terminator
eol :: GenParser Char st String
eol =     try (string "\n\r")
      <|> try (string "\r\n")
      <|> string "\n"
      <|> string "\r"
      <?> "end of line"

-- | quick & dirty integer
integer :: GenParser Char st String
integer = many digit

-- | PDB serial numbers
serialNumber :: GenParser Char st String
serialNumber = integer
