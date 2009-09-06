
module PDB.Format.Sections.Header where

import PDB.Format.Types
import PDB.Format.Peano


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
      record         :: RecordName
    , classification :: StringN P40
    , date           :: Date
    , idCode         :: IDcode
    }