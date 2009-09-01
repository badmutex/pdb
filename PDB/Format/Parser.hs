{-# LANGUAGE
  FlexibleInstances
  , NoMonomorphismRestriction
  , TypeSynonymInstances
  #-}

module PDB.Format.Parser where

import PDB.Format.Types

import Text.ParserCombinators.Parsec
import Data.Monoid
import Data.Char

-- | Platform (Windows, *nix) agnostic line terminator
eol :: Parser String
eol =     try (string "\n\r")
      <|> try (string "\r\n")
      <|> string "\n"
      <|> string "\r"
      <?> "end of line"

instance Monoid (Parser String) where
    mempty = option "" (many anyChar)
    mappend p1 p2 = do
      p1' <- p1
      p2' <- p2
      return $ p1' ++ p2'

(<++>) :: (Monoid a) => a -> a -> a
l <++> r = mconcat [l,r]



positiveInt = many digit
negativeInt = option "" (string "-") <++> positiveInt

-- | Parse (-/+) integers
integral :: (Integral i, Read i) => Parser i
integral = (negativeInt <|> positiveInt) >>= return . read





achar :: Parser AChar
achar =
    (letter <?> "achar: letter")
    >>= return . AChar

atom :: Parser Atom
atom =
    (many letter <?> "atom: letters")
    >>= return . AtomName

character :: Parser Character
character =
    (noneOf "\0'\a\b\t\n\f\v\r" <?> "character: no ascii control chars")
    >>= return . Character

continuation :: Parser Continuation
continuation =
    (try (count 2 (char ' ')) <|> (count 2 digit) <?> "continuation: 2 spaces or 2 digits")
    >>= return . Continuation

date :: Parser Date
date = do
  d <- count 2 digit
  char '-'
  m <- monthP
  char '-'
  y0 <- digit
  y1 <- digit
  return $ Date { day = read d
                , month = m
                , year = readYear y0 y1 }
      where
        readYear '0' y = 2000 + read [y]
        readYear '1' y = readYear '0' y + 10
        readYear  c  y = 1900 + read [c,y]

        monthP :: Parser Int
        monthP = let months = [ ("Jan",1), ("Feb",2), ("Mar",3), ("Apr",4)
                              , ("May",5), ("Jun",6), ("Jul",7), ("Aug",8)
                              , ("Sep",9), ("Oct",10), ("Nov",11), ("Dec",12) ]

                     p :: [(String, Int)] -> Parser Int
                     p = foldr1 (<|>) . map (try . pmonth)

                     pmonth :: (String,Int) -> Parser Int
                     pmonth (m,c) = string m >> return c

                     mapMonths f = map (\(m,c) -> (map f m, c))
                     upper = mapMonths toUpper
                     lower = mapMonths toLower
                 in p $ upper months ++ lower months ++ months
