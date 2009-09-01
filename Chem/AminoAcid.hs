{-# LANGUAGE
  RankNTypes
  #-}

module Chem.AminoAcid where

import Chem.Atom
import Chem.Molecule

import Data.Graph.Inductive.Graph (Node)


data AminoAcidSymbols = AminoAcidSymbols {
      name      :: String
    , shorthand :: String
    , symbol    :: Char
    } deriving (Eq, Read, Show)

data AminoAcid = AminoAcid {
      symbols        :: AminoAcidSymbols
    , molecule       :: Molecule m => m
    , nTerminal      :: Node
    , cTerminal      :: Node
    , carboxylCarbon :: Node
    }
