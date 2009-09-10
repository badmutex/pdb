module PDB.Format.Sections.Hetnam where

import PDB.Format.Types
import PDB.Format.ParsecMisc

import Text.ParserCombinators.Parsec

type Hetnam = ()

hetnamLine :: Parser Hetnam
hetnamLine = do
  string "HETNAM"
  anyChar `manyTill` eol
  return ()

hetnam :: Parser Hetnam
hetnam = ignoreSection hetnamLine
