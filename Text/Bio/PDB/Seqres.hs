module Text.Bio.PDB.Seqres ( -- * Comments have been taken from the PDB documentation

                             fieldName
                           , serNum
                           , chainId
                           , numRes
                           , resName
                           , theResidues
                           , seqres
                           ) where

import Text.Bio.PDB.Misc
import Text.Bio.PDB.Residue

import Text.ParserCombinators.Parsec

-- | The header name for the field
fieldName = string "SEQRES"

-- | The serial number of the SEQRES record from the current chain.
-- Starts at 1 and increments by 1 each line. Reset to 1 for each chain.
serNum = do
  fieldName ; spaces
  integer

-- | Chain identifier. This may be a single legal character, including
-- a blank which is used if there is only one chain
chainId = do
  serNum ; spaces
  anyChar

-- | Number of residues in the chain. This value is repeated on every record.
numRes = do
  chainId ; spaces
  integer

-- | Residue name. Alias for 'Text.Bio.PDB.Residue.residue'
resName = residue

-- | All the residue in the line from the current point.
-- Alias for 'Text.Bio.PDB.Residue.residues'
theResidues = residues

-- | get the sequence of residues for the current line.
seqres = do
  fieldName    ; spaces
  serialNumber ; spaces
  letter       ; spaces
  serialNumber ; spaces
  residues

