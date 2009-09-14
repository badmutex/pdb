module PDB.Format.Sections.Helix where

import PDB.Chemistry
import PDB.Format.Types
import PDB.Format.Instances
import qualified PDB.Format.ParserTools as P
import PDB.Format.ParsecMisc
import qualified PDB.Format.Peano as Peano

import Text.ParserCombinators.Parsec
import Prelude hiding (length)

-- |
-- >        CLASS NUMBER 
-- >        TYPE OF HELIX           (COLUMNS 39 - 40) 
-- > -------------------------------------------------------------- 
-- > Right-handed alpha (default)                1 
-- > Right-handed omega                          2 
-- > Right-handed pi                             3 
-- > Right-handed gamma                          4 
-- > Right-handed 310                            5 
-- > Left-handed alpha                           6 
-- > Left-handed omega                           7 
-- > Left-handed gamma                           8 
-- > 27 ribbon/helix                             9 
-- > Polyproline                                10 

data HelixClass = RightHandedAlpha
                | RightHandedOmega
                | RightHandedPi
                | RightHandedGamma
                | RightHanded310
                | LeftHandedAlpha
                | LeftHandedOmega
                | LeftHandedGamma
                | RibbonHelix27
                | Polyproline
                  deriving (Eq, Read, Show)

mkHelixClass 1  = RightHandedAlpha
mkHelixClass 2  = RightHandedOmega
mkHelixClass 3  = RightHandedPi
mkHelixClass 4  = RightHandedGamma
mkHelixClass 5  = RightHanded310
mkHelixClass 6  = LeftHandedAlpha
mkHelixClass 7  = LeftHandedOmega
mkHelixClass 8  = LeftHandedGamma
mkHelixClass 9  = RibbonHelix27
mkHelixClass 10 = Polyproline

data HelixLine = HL {
      serNum      :: Integer           -- ^ Serial number of the helix. 
                                       --   Starts at 1 and increases incrementally
    , helixId     :: LStringN Peano.P3 -- ^ Helix identifier. In addition to a serial number,
                                       --   each helix is given an alphanumeric character
                                       --   helix identifier .
    , initResName :: ResidueName       -- ^ Name of the initial residue
    , initChainId :: Char              -- ^ Chain identifier for the chain containing 
                                       --   this helix
    , initSeqNum  :: Integer           -- ^ Sequence number of the initial residue
    , initICode   :: Maybe Char        -- ^ Chain identifier for the chain containing 
                                       --   this helix
    , endResName  :: ResidueName       -- ^ Name of the terminal residue of the helix
    , endChainId  :: Char              -- ^ Chain identifier for the chain containing 
                                       --   this helix
    , endSeqNum   :: Integer           -- ^ Sequence number of the terminal residue
    , endICode    :: Maybe Char        -- ^ Insertion code of the terminal residue
    , helixClass  :: HelixClass        -- ^ Helix class (see above)
    , comment     :: String            -- ^ Comment about this helix
    , length      :: Integer           -- ^ Length of this helix
    } deriving Show


helixline :: Parser HelixLine
helixline = do
  string "HELIX"                          ; spaces
  sernum      <- P.pdbinteger             ; space
  helixid     <- P.lstringn Peano.p3      ; space
  initresname <- P.residuename            ; space
  initchainid <- anyChar                  ; spaces
  initseqnum  <- P.pdbinteger
  initicode   <- optionMaybe P.achar      ; spaces
  endresname  <- P.residuename            ; space
  endchainid  <- anyChar                  ; spaces
  endseqnum   <- P.pdbinteger
  endicode    <- optionMaybe P.achar      ; spaces
  helixclass  <- P.pdbinteger             ; space
  com         <- count 30 anyChar         ; spaces
  len         <- P.pdbinteger

  return $ HL {
               serNum      = from sernum
             , helixId     = helixid
             , initResName = initresname
             , initChainId = initchainid
             , initSeqNum  = from initseqnum
             , initICode   = from `fmap` initicode
             , endResName  = endresname
             , endChainId  = endchainid
             , endSeqNum   = from endseqnum
             , endICode    = from `fmap` endicode
             , helixClass  = mkHelixClass $ from helixclass
             , comment     = com
             , length      = from len
             }
  