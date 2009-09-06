module PDB.Format.Sections.Title where

import PDB.Format.Types
import qualified PDB.Format.Peano as Peano
import qualified PDB.Format.ParserTools as P
import PDB.Format.ParsecMisc

import Text.ParserCombinators.Parsec

-- | Ignore the title for now
type Title = ()

titleLine :: Parser Title
titleLine = do
  string "TITLE"
  anyChar `manyTill` eol
  return ()

title :: Parser Title
title = do
  skipMany1 titleLine
