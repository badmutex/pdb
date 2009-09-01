module Chem.Types where



class Atom a where
    readAtom :: String -> a
    
class Molecule a

class Residue a where
    readResidue :: String -> a