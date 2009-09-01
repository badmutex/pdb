module PDB.Chemistry where

class Atom a where
    readAtom :: String -> a

class Molecule a where
    readMolecule :: String -> a

class Residue a where
    readResidue :: String -> a
