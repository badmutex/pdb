module Chem.AtomsList where

import Chem.Atom

mke n s ps ns = Element {
                   symbols   = ElementSymbol {name = n, symb = s}
                 , protons   = ps
                 , neutrons  = ns
                 }

hydrogen = mke "Hydrogen" "H" 1 1
helium   = mke "Helium"   "He" 2 2
boron    = mke "Boron"    "B" 5 5
carbon   = mke "Carbon"   "C" 6 6
nitrogen = mke "Nitrogen" "N" 7 7
oxygen   = mke "Oxygen"   "O" 8 8
sulfur   = mke "Sulfur"   "S" 16 16
