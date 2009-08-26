module Text.Bio.PDB.Parser where

import Text.ParserCombinators.Parsec

import Control.Monad
import Data.Maybe

eol =     try (string "\n\r")
      <|> try (string "\r\n")
      <|> string "\n"
      <|> string "\r"
      <?> "end of line"

residue =     try (string "ALA")
          <|> try (string "ARG")
          <|> try (string "ASN")
          <|> try (string "ASN")
          <|> try (string "ASP")
          <|> try (string "CYS")
          <|> try (string "GLN")
          <|> try (string "GLU")
          <|> try (string "GLY")
          <|> try (string "HIS")
          <|> try (string "ILE")
          <|> try (string "LEU")
          <|> try (string "LYS")
          <|> try (string "MET")
          <|> try (string "PHE")
          <|> try (string "PRO")
          <|> try (string "SER")
          <|> try (string "THR")
          <|> try (string "TRP")
          <|> try (string "TYR")
          <|> try (string "VAL")
          <?> "capitalized three-letter amino-acid symbols"

residues = sepEndBy residue spaces

seqresHead = string "SEQRES"

seqres = do
  seqresHead ; spaces
  many digit ; spaces
  letter     ; spaces
  many digit ; spaces
  residues

data AA = AA String deriving Show

aas s = let as = parse seqres "" s
      in case as of
           Left e -> Nothing
           Right as' -> Just $ map AA as'

main = do
  ls <- fmap lines $ readFile "/Users/badi/src/pdb.git/Test/2VVA-SEQRES.txt"
  let as = foldl (\as' s -> if isJust s then fromJust s : as' else as') [] $ map aas ls
  return $ concat as