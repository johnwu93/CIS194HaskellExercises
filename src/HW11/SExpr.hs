{-# OPTIONS_GHC -Wall #-}

module HW11.SExpr where

import Control.Applicative ((<|>))
import Data.Char (isAlpha, isAlphaNum, isSpace)
import HW10.AParser (Parser(..), satisfy, posInt, runParser)
import Data.Maybe (isNothing)

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore parser = ((:) <$> parser <*> zeroOrMore parser) <|> pure []

oneOrMore :: Parser a -> Parser [a]
oneOrMore parser = (:) <$> parser <*> zeroOrMore parser

spaces :: Parser String
spaces = zeroOrMore (satisfy isSpace)

ident :: Parser String
ident = (++) <$> oneOrMore (satisfy isAlpha) <*> zeroOrMore (satisfy isAlphaNum)

toNumber :: [Integer] -> Integer
toNumber numList = sum $ zipWith (\x y -> x * (10 ^ y) )(reverse numList) [0,1..]

-- todo a monad would make implementing this alot easier
excludeRule :: Parser a -> Parser b -> Parser a
excludeRule desiredParser@(Parser f) unwantedParser =
  let subResult = (\_ _ -> ()) <$> desiredParser <*> unwantedParser in
    Parser (\oldString -> if isNothing (runParser subResult oldString) then f oldString else Nothing)

number :: Parser Integer
number = excludeRule (toNumber <$> oneOrMore posInt) ident

type Ident = String

data Atom = N Integer | I Ident
  deriving Show


data SExpr = A Atom | Comb [SExpr]
  deriving Show

atom :: Parser Atom
atom = fmap I ident <|> fmap N number

removeSpaceFromAtom :: Parser Atom
removeSpaceFromAtom = spaces *> atom <* spaces

eof :: Parser SExpr
eof = Parser parseEof
    where parseEof [] = Just (Comb [],[])
          parseEof _  = Nothing


convertAtomToSExpr :: Parser SExpr
convertAtomToSExpr = fmap A removeSpaceFromAtom

parseSExpr :: Parser SExpr
parseSExpr = eof <|> (\ left right -> Comb [left, right]) <$> convertAtomToSExpr <*> parseSExpr
