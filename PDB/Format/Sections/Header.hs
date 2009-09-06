module PDB.Format.Sections.Header where

import PDB.Format.Types
import qualified PDB.Format.Peano as Peano
import qualified PDB.Format.ParserTools as P
import PDB.Format.ParsecMisc

import Text.ParserCombinators.Parsec


-- |
-- @COLUMNS       DATA  TYPE     FIELD             DEFINITION@
--
-- @------------------------------------------------------------------------------------@
--
--  @1 -  6       Record name    "HEADER"@
--
-- @11 - 50       String(40)     classification    Classifies the molecule(s).@
--
-- @51 - 59       Date           depDate           Deposition date. This is the date the
--                                               coordinates  were received at the PDB.@
--
-- @63 - 66       IDcode         idCode            This identifier is unique within the PDB.@
--

data Header = Header {
      classification :: StringN Peano.P40
    , date           :: Date
    , idcode         :: IDcode
    } deriving (Read, Show)

header :: Parser Header
header = do
  string "HEADER"
  spaces
  c <- P.stringn 40
  d <- P.date
  spaces
  i <- P.idcode
  eol
  return Header { classification = c
                , date           = d
                , idcode         = i
                }
