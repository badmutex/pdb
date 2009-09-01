{-# LANGUAGE NoMonomorphismRestriction #-}

module Text.Bio.PDB.Sheet where

import Chem.Chemistry
import Text.Bio.PDB.Misc
import Text.Bio.PDB.Residue

import Text.ParserCombinators.Parsec


fieldName = string "SHEET"

strand :: GenParser Char st Int
strand = do
  fieldName ; spaces
  integer
  >>= readM

sheetId :: GenParser Char st String
sheetId = do
  strand ; spaces
  many letter

numStrands :: GenParser Char st Int
numStrands = do
  sheetId ; spaces
  integer
  >>= readM

initResName :: GenParser Char st String
initResName = do
  numStrands ; spaces
  residue

initResidue :: (Residue r) => GenParser Char st r
initResidue = initResName >>= readResidueM

initChainId :: GenParser Char st Char
initChainId = do
  initResName ; spaces
  anyChar

initSeqNum :: GenParser Char st Integer
initSeqNum = do
  initChainId ; spaces
  integer
  >>= readM

initICode :: GenParser Char st Char
initICode = do
  initSeqNum
  anyChar

endResName :: GenParser Char st String
endResName = do
  initICode ; spaces
  residue

endResidue :: (Residue r) => GenParser Char st r
endResidue = endResName >>= readResidueM

endChainId :: GenParser Char st Char
endChainId = do
  endResName ; space
  anyChar

endSeqNum :: GenParser Char st Integer
endSeqNum = do
  endChainId ; spaces
  integer
  >>= readM

endICode :: GenParser Char st Char
endICode = do
  endSeqNum
  anyChar

sense :: GenParser Char st Int
sense = do
  endICode ; spaces
  integer 
  >>= readM




registration_curAtom =
    many letter

registration_curResName = do
  many letter >>= readResidueM

registration_curChainId =
    anyChar

registration_curResSeq = do
  integer >>= readM

registration_curICode =
    letter

registration_prevAtom =
    many letter

registration_prevResName =
    many letter

registration_prevChainId =
    letter

registration_prevResSeq =
    integer >>= readM

registration_prevICode =
    anyChar

registration = do
  registration_curAtom
