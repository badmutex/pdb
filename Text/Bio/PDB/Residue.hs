{-# LANGUAGE NoMonomorphismRestriction #-}

module Text.Bio.PDB.Residue where

import Chem.Chemistry

import Text.ParserCombinators.Parsec

-- | The 20 amino acids potentially found in a PDB file.
residue :: GenParser Char st String
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

-- | test
residues :: Residue r => GenParser Char st [r]
residues = do
  r <- residue `sepEndBy` spaces
  return $ map readResidue r
