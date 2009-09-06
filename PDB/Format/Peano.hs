{-# LANGUAGE
  EmptyDataDecls
  , FlexibleInstances
  , FunctionalDependencies
  , MultiParamTypeClasses
  , UndecidableInstances
  #-}

{-# OPTIONS_GHC -fcontext-stack=60 #-}

-- | Some Peano numbers and shortcuts for use by
-- phantom types (ie 'LStringN') in 'PDB.Format.Types'.
--
-- None of this code is originally mine (though I have modifed it
-- a bit). It's been gathered from the recesses of Google as I
-- tried to understand Peano numbers and embedding their algebra
-- in the type system.
module PDB.Format.Peano where

import Prelude hiding (pred)
import Data.List


peano_previous :: Succ a -> a
peano_previous = const undefined

class Count a where
    count :: a -> Int

instance Count P0 where
    count = const 0
instance Count a => Count (Succ a) where
    count a = 1 + count (peano_previous a)

class Add a b c | a b -> c where
    add :: a -> b -> c

instance Add P0 b b
instance Add a b c => Add (Succ a) b (Succ c)


data Succ a

data P0
type P1  = Succ P0
type P2  = Succ P1
type P3  = Succ P2
type P4  = Succ P3
type P5  = Succ P4
type P6  = Succ P5
type P7  = Succ P6
type P8  = Succ P7
type P9  = Succ P8
type P10 = Succ P9
type P11 = Succ P10
type P12 = Succ P11
type P13 = Succ P12
type P14 = Succ P13
type P15 = Succ P14
type P16 = Succ P15
type P17 = Succ P16
type P18 = Succ P17
type P19 = Succ P18
type P20 = Succ P19
type P21 = Succ P20
type P22 = Succ P21
type P23 = Succ P22
type P24 = Succ P23
type P25 = Succ P24
type P26 = Succ P25
type P27 = Succ P26
type P28 = Succ P27
type P29 = Succ P28
type P30 = Succ P29
type P31 = Succ P30
type P32 = Succ P31
type P33 = Succ P32
type P34 = Succ P33
type P35 = Succ P34
type P36 = Succ P35
type P37 = Succ P36
type P38 = Succ P37
type P39 = Succ P38
type P40 = Succ P39
type P41 = Succ P40
type P42 = Succ P41
type P43 = Succ P42
type P44 = Succ P43
type P45 = Succ P44
type P46 = Succ P45
type P47 = Succ P46
type P48 = Succ P47
type P49 = Succ P48
type P50 = Succ P49


p0  = undefined :: P0
p1  = undefined :: P1
p2  = undefined :: P2
p3  = undefined :: P3
p4  = undefined :: P4
p5  = undefined :: P5
p6  = undefined :: P6
p7  = undefined :: P7
p8  = undefined :: P8
p9  = undefined :: P9
p10 = undefined :: P10
p11 = undefined :: P11
p12 = undefined :: P12
p13 = undefined :: P13
p14 = undefined :: P14
p15 = undefined :: P15
p16 = undefined :: P16
p17 = undefined :: P17
p18 = undefined :: P18
p19 = undefined :: P19
p20 = undefined :: P20
p21 = undefined :: P21
p22 = undefined :: P22

p30 = undefined :: P30
p40 = undefined :: P40
p50 = undefined :: P50
