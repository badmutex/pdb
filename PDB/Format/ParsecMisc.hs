{-# LANGUAGE
  FlexibleInstances
  , NoMonomorphismRestriction
  , TypeSynonymInstances
  #-}

-- | Extra parsers and combinators to supplements those of Parsec.
module PDB.Format.ParsecMisc where

import Text.ParserCombinators.Parsec
import Data.Monoid


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


decimal :: (Read f, Fractional f) => Parser f
decimal = do
  ds <- many digit
  char '.'
  ds' <- many digit
  return . read $ ds ++ "." ++ ds'
  <?> "a fractional number"


ignoreSection :: Parser a -> Parser ()
ignoreSection = skipMany1
