module Text.Bio.PDB.Helix where

import Text.Bio.PDB.Misc
import Text.Bio.PDB.Residue

import Text.ParserCombinators.Parsec
import Prelude hiding (length)

-- | The header name for the field
fieldName :: CharParser st String
fieldName  = string "HELIX"

-- | Serial number of the helix. This starts at 1  and increases incrementally.
serNum :: GenParser Char st String
serNum     = do
  fieldName  ; spaces
  serialNumber

-- | Helix  identifier. In addition to a serial number, each helix is given an 
-- alphanumeric character helix identifier.
helixId :: GenParser Char st String
helixId    = do
  serNum     ; spaces
  serialNumber

-- | Name of the initial residue.
resName :: GenParser Char st String
resName    = do
  helixId    ; spaces
  residue

-- | Chain identifier for the chain containing this  helix.
chainId :: GenParser Char st Char
chainId    = do
  resName    ; spaces
  anyChar

-- | Sequence number of the initial residue.
initSeqNum :: GenParser Char st String
initSeqNum = do
  chainId    ; spaces
  integer

-- | Insertion code of the initial residue.
initICode :: GenParser Char st Char
initICode  = do
  initSeqNum
  anyChar

-- | Name of the terminal residue of the helix
endResName :: GenParser Char st String
endResName = do
  initICode  ; spaces
  residue

-- | Chain identifier for the chain containing this  helix
endChainId :: GenParser Char st Char
endChainId = do
  endResName ; space
  anyChar

-- | Sequence number of the terminal residue
endSeqNum :: GenParser Char st String
endSeqNum  = do
  endChainId ; spaces
  integer

-- | Insertion code of the terminal residue
endICode :: GenParser Char st Char
endICode   = do
  endSeqNum
  anyChar

-- | Helix class
-- 
-- see <http://www.wwpdb.org/documentation/format32/sect5.html> 
-- under the details for the HELIX section for explanations on class numbers
helixClass :: GenParser Char st String
helixClass = do
  endICode   ; space
  integer

-- | Comment about this helix
comment :: GenParser Char st String
comment    = do
  helixClass ; space
  count 29 $ (anyChar <|> space)

-- | Length of this helix
length :: GenParser Char st String
length     = do
  comment    ; spaces
  integer
