module Chem.Atom where

data Position = Position { 
      x :: Double
    , y :: Double
    , z :: Double
    } deriving (Eq, Read, Show)

newtype Charge        = Charge Double        deriving (Eq, Read, Show)
data ElementSymbol = ElementSymbol {
      name :: String
    , symb :: String
    } deriving (Eq, Read, Show)

data Element = Element {
      symbols   :: ElementSymbol
    , protons   :: Int
    , neutrons  :: Int
    } deriving (Eq, Read, Show)


data AtomT = Atom {
      at_element  :: Element
    , at_position :: Maybe Position
    , at_charge   :: Maybe Charge
    , at_extra    :: [String]
    } deriving (Eq, Read, Show)


class Atom a where
    element :: a -> Element
    position :: a -> Maybe Position
    charge :: a -> Maybe Charge

instance Atom AtomT where
    element = at_element
    position = at_position
    charge = at_charge