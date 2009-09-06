{-# LANGUAGE
  FlexibleInstances
  , FunctionalDependencies
  , MultiParamTypeClasses
  , TypeSynonymInstances
  #-}

module PDB.Format.Instances where

import PDB.Format.Types

instance From AChar Char                        where from (AChar c)        = c
instance From Atom String                       where from (AtomName s)     = s
instance From Character Char                    where from (Character c)    = c
instance From Continuation String               where from (Continuation s) = s
instance From IDcode String                     where from (IDcode s)       = s
instance From PDBInteger Integer                where from (PDBInteger i)   = i
instance From Token String                      where from (Token s)        = s
instance From List [String]                     where from (List l)         = l
instance From LString String                    where from (LString s)      = s
instance From (LStringN a)String                where from (LStringN s)     = s
instance From PDBReal Double                    where from (PDBReal d)      = d
instance From RecordName String                 where from (RecordName s)   = s
instance From ResidueName String                where from (ResidueName s)  = s
instance From SList [String]                    where from (SList l)        = l
instance From SpecificationList [Specification] where from (SpecList l)     = l
instance From PDBString String                  where from (PDBString s)    = s
instance From (StringN a) String                where from (StringN s)      = s
instance From SymOP Int                         where from (SymOP i)        = i
