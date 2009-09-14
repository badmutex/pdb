{-# LANGUAGE
  EmptyDataDecls
  , FlexibleContexts
  , FunctionalDependencies
  , MultiParamTypeClasses
  , RankNTypes
  , TypeSynonymInstances
  #-}

-- | 
-- @DATA TYPE               DESCRIPTION@
-- 
-- @----------------------------------------------------------------------------------@
-- 
-- @AChar                   An alphabetic character (A-Z, a-z).@
-- 
-- @Atom                    Atom name.@ 
-- 
-- @Character               Any non-control character in the  ASCII character set or a
--                         space.@
-- 
-- @Continuation            A two-character field that is either blank (for the first
--                        record of a set) or contains a two  digit number
--                        right-justified and blank-filled which counts continuation
--                        records starting with 2. The continuation number must be
--                        followed by a blank.@
-- 
-- @Date                    A 9 character string in the form DD-MMM-YY  where DD is the
--                        day of the month, zero-filled on the left  (e.g., 04); MMM is
--                        the common English 3-letter  abbreviation of the month; and
--                        YY is the last two digits of the year. This must represent
--                        a valid date.@
-- 
-- @IDcode                  A PDB identification code which  consists of 4 characters,
--                        the first of which is a digit  in the range 0 - 9; the
--                        remaining 3 are  alpha-numeric, and letters are upper case
--                        only. Entries with a 0 as  the first character do not
--                        contain coordinate data.@
-- 
-- @Integer                 Right-justified blank-filled integer  value.@
-- 
-- @Token                   A sequence of non-space characters  followed by a colon and a
--                        space.@
-- 
-- @List                    A String that is composed of text  separated with commas.@
-- 
-- @LString                 A literal string of characters. All spacing  is significant
--                        and must be preserved.@
-- 
-- @LString(n)              An LString with exactly n characters.@
-- 
-- @Real(n,m)               Real (floating point) number in the  FORTRAN format Fn.m.@
-- 
-- @Record name             The name of the record: 6 characters,  left-justified and
--                        blank-filled.@
-- 
-- @Residue name            One of the standard amino acid or nucleic acids, as listed
--                        below, or the non-standard group  designation as defined in
--                        the HET dictionary. Field is  right-justified.@
-- 
-- @SList                   A String that is composed of text  separated with semi-colons.@
-- 
-- @Specification           A String composed of a token and its  associated value
--                        separated by a colon.@
-- 
-- @Specification List      A sequence of Specifications, separated by semi-colons.@
-- 
-- @String                  A sequence of characters. These  characters may have
--                        arbitrary spacing, but should be  interpreted as directed
--                        below.@
-- 
-- @String(n)               A String with exactly n characters.@
-- 
-- @SymOP                   An integer field of from 4 to 6  digits, right-justified, of
--                        the form nnnMMM where nnn is the symmetry  operator number and
--                        MMM is the translation vector.@ 

module PDB.Format.Types where

import PDB.Chemistry

-- | An alphabetic character (A-Z, a-z)
newtype AChar = AChar Char
    deriving (Eq, Ord, Read, Show)

-- | Atom name
newtype Atom = AtomName String
    deriving (Eq, Ord, Read, Show)

-- | Any non-control character in the  ASCII character set or a space
newtype Character = Character Char
    deriving (Eq, Ord, Read, Show)

-- | A two-character field that is either blank (for the first
-- record of a set) or contains a two  digit number
-- right-justified and blank-filled which counts continuation
-- records starting with 2. The continuation number must be
-- followed by a blank.
newtype Continuation = Continuation String
    deriving (Eq, Ord, Read, Show)

-- | A 9 character string in the form DD-MMM-YY  where DD is the
-- day of the month, zero-filled on the left  (e.g., 04); MMM is
-- the common English 3-letter  abbreviation of the month; and
-- YY is the last two digits of the year.  This must represent
-- a valid date.
data Date = Date {
      day   :: Int
    , month :: Int
    , year  :: Int
    }
    deriving (Eq, Ord, Read, Show)

-- | A PDB identification code which  consists of 4 characters,
-- the first of which is a digit  in the range 0 - 9; the
-- remaining 3 are  alpha-numeric, and letters are upper case
-- only. Entries with a 0 as  the first character do not
-- contain coordinate data.
newtype IDcode = IDcode String
    deriving (Eq, Ord, Read, Show)

-- | Right-justified blank-filled integer  value.
newtype PDBInteger = PDBInteger Integer
    deriving (Eq, Ord, Read, Show)

-- | A sequence of non-space characters  followed by a colon and a space
newtype Token = Token String
    deriving (Eq, Ord, Read, Show)

-- | A String that is composed of text  separated with commas.
newtype List = List [String]
    deriving (Eq, Ord, Read, Show)

-- | A literal string of characters. All spacing  is significant
-- and must be preserved.
newtype LString = LString String
    deriving (Eq, Ord, Read, Show)

-- | An LString with exactly n characters.
newtype LStringN a = LStringN String
    deriving (Eq, Ord, Read, Show)

-- | Real (floating point) number in the  FORTRAN format Fn.m.
newtype PDBReal = PDBReal Double
    deriving (Eq, Ord, Read, Show)

-- | The name of the record: 6 characters,  left-justified and
-- blank-filled.
newtype RecordName = RecordName String
    deriving (Eq, Ord, Read, Show)

-- | One of the standard amino acid or nucleic acids, as listed
-- below, or the non-standard group  designation as defined in
-- the HET dictionary. Field is right-justified.
newtype ResidueName = ResidueName String
    deriving (Eq, Ord, Read, Show)

-- | A String that is composed of text  separated with semi-colons.
newtype SList = SList [String]
    deriving (Eq, Ord, Read, Show)

-- | A String composed of a token and its  associated value
-- separated by a colon.
data Specification = Spec {
      specToken :: Token
    , specValue :: String
    }
    deriving (Eq, Ord, Read, Show)

-- | A sequence of Specifications, separated by semi-colons.
newtype SpecificationList = SpecList [Specification]
    deriving (Eq, Ord, Read, Show)

-- | A sequence of characters. These  characters may have
-- arbitrary spacing, but should be  interpreted as directed
-- below.
newtype PDBString = PDBString String
    deriving (Eq, Ord, Read, Show)

-- | A String with exactly n characters.
newtype StringN a = StringN String
    deriving (Eq, Ord, Read, Show)

-- | An integer field of from 4 to 6  digits, right-justified, of
-- the form nnnMMM where nnn is the symmetry  operator number and
-- MMM is the translation vector. 
newtype SymOP = SymOP Int
    deriving (Eq, Ord, Read, Show)



-- | A generic way to extract the data from the above data types.
--
-- @from (IDcode 42) == 42@
class From a b | a -> b where from :: a -> b

type ID = Integer

class Residue r =>
    PrimaryStructure s r where
        mkPrimaryStructure :: [r] -> s r
