module Chem.AminoAcidsList where

import Chem.AminoAcid

mkSymbol n sh s = AminoAcidSymbols n sh s
mkaa = mkSymbol

alanine_symbol       = mkaa "alanine"       "ala" 'a'
arginine_symbol      = mkaa "arginine"      "arg" 'r'
asparagine_symbol    = mkaa "asparagine"    "asn" 'n'
asparticAcid_symbol  = mkaa "aspartic acid" "asp" 'd'
cysteine_symbol      = mkaa "cysteine"      "cys" 'c'
glutamine_symbol     = mkaa "glutamine"     "gln" 'q'
glutamicAcid_symbol  = mkaa "glutamic acid" "glu" 'e'
glycine_symbol       = mkaa "glycine"       "gly" 'g'
histidine_symbol     = mkaa "histidine"     "his" 'h'
isoleucine_symbol    = mkaa "isoleucine"    "ile" 'i'
leucine_symbol       = mkaa "leucine"       "leu" 'l'
lysine_symbol        = mkaa "lysine"        "lys" 'k'
methionine_symbol    = mkaa "methionine"    "met" 'm'
phenylalanine_symbol = mkaa "phenylalanine" "phe" 'f'
proline_symbol       = mkaa "proline"       "pro" 'p'
serine_symbol        = mkaa "serine"        "ser" 's'
threonine_symbol     = mkaa "threonine"     "thr" 't'
tryptophan_symbol    = mkaa "tryptophan"    "trp" 'w'
tyrosine_symbol      = mkaa "tyrosine"      "tyr" 'y'
valine_symbol        = mkaa "valine"        "val" 'v'