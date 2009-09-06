
module PDB.Format.Parser where

import PDB.Format.Types
import PDB.Format.ParsecMisc
import qualified PDB.Format.Peano as Peano

import Text.ParserCombinators.Parsec hiding (token)
import Data.Char


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


idcode :: Parser IDcode
idcode = do
  d <- digit
  cs <- count 3 (digit <|> upper)
  return $ IDcode (d : cs)

pdbinteger :: Parser PDBInteger
pdbinteger = many1 digit >>= return . PDBInteger . read

token :: Parser Token
token = noneOf [' '] `manyTill` string ": " >>= return . Token


list :: Parser List
list = many (anyChar `manyTill` end) >>= return . List
    where end = char ',' <|> newline

lstring :: Int -> Parser LString
lstring s = count s anyChar >>= return . LString

lstringn :: Int -> Parser (LStringN peanoNum)
lstringn s = count s anyChar >>= return . LStringN


pdbreal :: Parser PDBReal
pdbreal = decimal >>= return . PDBReal

recordname :: Parser RecordName
recordname = choice cs >>= return . RecordName
    where cs = map (\c -> try $ count c letter) (reverse [1..6])


residuename :: Parser ResidueName
residuename = residue >>= return . ResidueName
    where 
      -- | The 20 amino acids potentially found in a PDB file.
      residue =     try (string "ALA")
                <|> try (string "ARG")
                <|> try (string "ASN")
                <|> try (string "ASN")
                <|> try (string "ASP")
                <|> try (string "CYS")
                <|> try (string "GLN")
                <|> try (string "GLU")
                <|> try (string "GLY")
                <|> try (string "HIS")
                <|> try (string "ILE")
                <|> try (string "LEU")
                <|> try (string "LYS")
                <|> try (string "MET")
                <|> try (string "PHE")
                <|> try (string "PRO")
                <|> try (string "SER")
                <|> try (string "THR")
                <|> try (string "TRP")
                <|> try (string "TYR")
                <|> try (string "VAL")
                <?> "capitalized three-letter amino-acid symbols"

-- | TODO: implement
slist :: Parser SList
slist = undefined

-- | TODO: implement
specification :: Parser Specification
specification = undefined


specificationlist :: Parser SpecificationList
specificationlist = specification `manyTill` char ';' >>= return . SpecList


pdbstring :: Parser PDBString
pdbstring = undefined