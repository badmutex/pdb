module PDB.Format.Sections.Het where

import PDB.Format.Types
import PDB.Format.ParsecMisc

import Text.ParserCombinators.Parsec

type Het = ()

hetLine :: Parser Het
hetLine = do
  string "HET"
  anyChar `manyTill` eol
  return ()

het :: Parser Het
het = ignoreSection hetLine
