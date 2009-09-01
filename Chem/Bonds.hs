module Chem.Bonds where

data Bond = Single | Double | Triple | Amide | Aromatic | Disulfide
          deriving (Eq, Read, Show)