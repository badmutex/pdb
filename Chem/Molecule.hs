{-# LANGUAGE
  RankNTypes
  #-}

module Chem.Molecule where

import Chem.Atom
import Chem.Bonds

import Data.Graph.Inductive.Graph

data GraphMolecule = GraphMolecule (Graph g => g Atom Bond)

class Molecule a where

instance Molecule GraphMolecule where