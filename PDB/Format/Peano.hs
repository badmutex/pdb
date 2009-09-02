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
module PDB.Format.Peano where

import Prelude hiding (pred)

data Zero 
data Succ a

pred :: Succ a -> a
pred = const undefined

class Count a where
    count :: a -> Int

instance Count Zero where
    count = const 0
instance Count a => Count (Succ a) where
    count a = count (pred a) + 1

class Add a b c | a b -> c where
    add :: a -> b -> c

instance Add Zero b b
instance Add a b c => Add (Succ a) b (Succ c)



type One         = Succ Zero
type Two         = Succ One
type Three       = Succ Two
type Four        = Succ Three
type Five        = Succ Four
type Six         = Succ Five
type Seven       = Succ Six
type Eight       = Succ Seven
type Nine        = Succ Eight
type Ten         = Succ Nine
type Eleven      = Succ Ten
type Twelve      = Succ Eleven
type Thirteen    = Succ Twelve
type Fourteen    = Succ Thirteen
type Fifteen     = Succ Fourteen
type Sixteen     = Succ Fifteen
type Seventeen   = Succ Sixteen
type Eighteen    = Succ Seventeen
type Nineteen    = Succ Eighteen
type Twenty      = Succ Nineteen
type TwentyOne   = Succ Twenty
type TwentyTwo   = Succ TwentyOne
type TwentyThree = Succ TwentyTwo
type TwentyFour  = Succ TwentyThree
type TwentyFive  = Succ TwentyFour
type TwentySix   = Succ TwentyFive
type TwentySeven = Succ TwentySix
type TwentyEight = Succ TwentySeven
type TwentyNine  = Succ TwentyEight
type Thirty      = Succ TwentyNine
type ThirtyOne   = Succ Thirty
type ThirtyTwo   = Succ ThirtyOne
type ThirtyThree = Succ ThirtyTwo
type ThirtyFour  = Succ ThirtyThree
type ThirtyFive  = Succ ThirtyFour
type ThirtySix   = Succ ThirtyFive
type ThirtySeven = Succ ThirtySix
type ThirtyEight = Succ ThirtySeven
type ThirtyNine  = Succ ThirtyEight
type Forty       = Succ ThirtyNine
type FortyOne    = Succ Forty
type FortyTwo    = Succ FortyOne
type FortyThre   = Succ FortyTwo
type FortyFour   = Succ FortyThre
type FortyFive   = Succ FortyFour
type FortySix    = Succ FortyFive
type FortySeven  = Succ FortySix
type FortyEight  = Succ FortySeven
type FortyNine   = Succ FortyEight
type Fifty       = Succ FortyNine


zero = undefined :: Zero
one = undefined :: One
two = undefined :: Two
three = undefined :: Three
four = undefined :: Four
five = undefined :: Five
six = undefined :: Six
seven = undefined :: Seven
eight = undefined :: Eight
nine = undefined :: Nine

twentytwo = undefined :: TwentyTwo
thirty = undefined :: Thirty
forty = undefined :: Forty
fifty = undefined :: Fifty

main = print (count fifty)