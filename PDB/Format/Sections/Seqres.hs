{-# LANGUAGE 
  MultiParamTypeClasses
  , NoMonomorphismRestriction

  , FlexibleInstances
  , TypeSynonymInstances
  #-}

-- | Example Usage
--
-- >  seqres =
-- >      "SEQRES   1 X  260  MET SER HIS HIS TRP GLY TYR GLY LYS HIS ASN GLY PRO\n" ++
-- >      "SEQRES   2 X  260  GLU HIS TRP HIS LYS ASP PHE PRO ILE ALA LYS GLY GLU\n" ++
-- >      "SEQRES   3 X  260  ARG GLN SER PRO VAL ASP ILE ASP THR HIS THR ALA LYS\n" ++
-- >      "SEQRES   4 X  260  TYR ASP PRO SER LEU LYS PRO LEU SER VAL SER TYR ASP\n" ++
-- >      "SEQRES   5 X  260  GLN ALA THR SER LEU ARG ILE LEU ASN ASN GLY HIS ALA\n" ++
-- >      "SEQRES   6 X  260  PHE ASN VAL GLU PHE ASP ASP SER GLN ASP LYS ALA VAL\n" ++
-- >      "SEQRES   7 X  260  LEU LYS GLY GLY PRO LEU ASP GLY THR TYR ARG LEU ILE\n" ++
-- >      "SEQRES   8 X  260  GLN PHE HIS PHE HIS TRP GLY SER LEU ASP GLY GLN GLY\n"
--
-- > data Residue r => PS r = PS [(Integer, r)] deriving Show
--
-- > instance Residue r => From (PS r) [(Integer, r)] where
-- >     from (PS ps) = ps
--
-- > instance PrimaryStructure PS String where
-- >     mkPrimaryStructure = PS . zip [1..]
--
-- > instance Residue String where
-- >     readResidue = id
--
-- > test :: Either ParseError (PS String)
-- > test = parse primarystructure "" seqres
--

module PDB.Format.Sections.Seqres ( PrimaryStructure (..)
                                  , primarystructure
                                  ) where

import PDB.Chemistry
import PDB.Format.Types
import PDB.Format.Instances
import qualified PDB.Format.ParserTools as P
import PDB.Format.ParsecMisc

import Text.ParserCombinators.Parsec


data Residue r => SeqresLine r = SL {
      serNum       :: Integer
    , chainId      :: Char
    , numRes       :: Integer
    , residuenames :: [r]
    } deriving Show

seqresline :: Residue r => Parser (SeqresLine r)
seqresline = let get = fmap from
             in do
               string "SEQRES" ; spaces
               i    <- get P.pdbinteger ; space
               cid  <- get P.character  ; spaces
               nres <- get P.pdbinteger ; spaces
               ress <- (map (readResidue . from)) 
                       `fmap`
                       (P.residuename `sepEndBy` (try (many space) <|> eol))
               return SL { serNum       = i
                         , chainId      = cid
                         , numRes       = nres
                         , residuenames = ress }


primarystructure :: (Residue r, PrimaryStructure s r) => Parser (s r)
primarystructure = do
  lines <- many seqresline
  return . mkPrimaryStructure . reverse $
         foldl work [] lines
             where work seq = flip (++) seq . reverse . residuenames


{-
-- DEBUG
seqres =
     "SEQRES   1 X  260  MET SER HIS HIS TRP GLY TYR GLY LYS HIS ASN GLY PRO\n" ++
     "SEQRES   2 X  260  GLU HIS TRP HIS LYS ASP PHE PRO ILE ALA LYS GLY GLU\n" ++
     "SEQRES   3 X  260  ARG GLN SER PRO VAL ASP ILE ASP THR HIS THR ALA LYS\n" ++
     "SEQRES   4 X  260  TYR ASP PRO SER LEU LYS PRO LEU SER VAL SER TYR ASP\n" ++
     "SEQRES   5 X  260  GLN ALA THR SER LEU ARG ILE LEU ASN ASN GLY HIS ALA\n" ++
     "SEQRES   6 X  260  PHE ASN VAL GLU PHE ASP ASP SER GLN ASP LYS ALA VAL\n" ++
     "SEQRES   7 X  260  LEU LYS GLY GLY PRO LEU ASP GLY THR TYR ARG LEU ILE\n" ++
     "SEQRES   8 X  260  GLN PHE HIS PHE HIS TRP GLY SER LEU ASP GLY GLN GLY\n"

data Residue r =>
    PS r = PS [(Integer, r)]
           deriving Show

instance Residue r =>
    From (PS r) [(Integer, r)] where
        from (PS ps) = ps

instance PrimaryStructure PS String where
    mkPrimaryStructure = PS . zip [1..]


instance Residue String where
    readResidue = id

test :: Either ParseError (PS String)
test = parse primarystructure "" seqres
-}
