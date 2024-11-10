{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Parsers
  ( Query (..),
    GrapeType (..),
    WineType (..),
    Quantity (..),
    BarrelType (..),
    Duration (..),
    parseTask,
    parseTaskList,
    skipSpaces,
    parseLiteral,
    parseChar,
    parseString,
    parseInt,
    char,
    Parser (..),
  )
where

import Control.Applicative (Alternative (empty), optional, (<|>))
import Data.Char (isDigit)

-- Data Types for Queries and State
data Query
  = Harvest GrapeType Quantity
  | Ferment GrapeType Duration
  | Age WineType Duration BarrelType
  | Bottle WineType Quantity
  | Sell WineType Quantity Double
  | View
  | WineFactory [Query]
  deriving (Eq, Show)

data GrapeType = CabernetSauvignon | Merlot | PinotNoir | Chardonnay
  deriving (Eq, Show)

data WineType = RedWine | WhiteWine | RoseWine
  deriving (Eq, Show)

data Quantity = Kg Int | L Int | Bottles Int
  deriving (Eq, Show)

data BarrelType = Oak | Steel | Clay
  deriving (Eq, Show)

data Duration = Days Int | Months Int
  deriving (Eq, Show)

newtype Parser a = P {parse :: String -> Either String (a, String)}

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f p = do
    a <- p
    return $ f a

instance Applicative Parser where
  pure :: a -> Parser a
  pure x = P $ \str -> Right (x, str)
  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  pf <*> pa = do
    f <- pf
    f <$> pa

instance Alternative Parser where
  empty :: Parser a
  empty = P $ \_ -> Left "Failed to parse"
  (<|>) :: Parser a -> Parser a -> Parser a
  (<|>) p1 p2 = P $ \str -> case parse p1 str of
    Right (v, r) -> Right (v, r)
    Left _ -> parse p2 str

instance Monad Parser where
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  (>>=) pa f = P $ \str -> case parse pa str of
    Left e -> Left e
    Right (a, r) -> parse (f a) r

-- Parsers for WineFactory Queries
parseTaskList :: Parser [Query]
parseTaskList = do
  firstQuery <- parseTask
  rest <- optional (char ';' >> parseTaskList)
  return $ case rest of
    Just otherQueries -> firstQuery : otherQueries
    Nothing -> [firstQuery]

parseTask :: Parser Query
parseTask =
  parseHarvest
    <|> parseFerment
    <|> parseAge
    <|> parseBottle
    <|> parseSell
    <|> parseView
    <|> parseWineFactory

-- <harvest> ::= "harvest" "(" <grape_type> "," <quantity> ")"
parseHarvest :: Parser Query
parseHarvest = do
  _ <- parseLiteral "harvest"
  _ <- parseChar '('
  gType <- parseGrapeType
  _ <- parseChar ','
  quantity <- parseQuantity
  _ <- parseChar ')'
  return $ Harvest gType quantity

-- <ferment> ::= "ferment" "(" <grape_type> "," <duration> ")"
parseFerment :: Parser Query
parseFerment = do
  _ <- parseLiteral "ferment"
  _ <- parseChar '('
  gType <- parseGrapeType
  _ <- parseChar ','
  duration <- parseDuration
  _ <- parseChar ')'
  return $ Ferment gType duration

-- <age> ::= "age" "(" <wine_type> "," <duration> "," <barrel_type> ")"
parseAge :: Parser Query
parseAge = do
  _ <- parseLiteral "age"
  _ <- parseChar '('
  wType <- parseWineType
  _ <- parseChar ','
  duration <- parseDuration
  _ <- parseChar ','
  barrelType <- parseBarrelType
  _ <- parseChar ')'
  return $ Age wType duration barrelType

-- <bottle> ::= "bottle" "(" <wine_type> "," <quantity> ")"
parseBottle :: Parser Query
parseBottle = do
  _ <- parseLiteral "bottle"
  _ <- parseChar '('
  wType <- parseWineType
  _ <- parseChar ','
  quantity <- parseQuantity
  _ <- parseChar ')'
  return $ Bottle wType quantity

-- <sell> ::= "sell" "(" <wine_type> "," <quantity> "," <price> ")"
parseSell :: Parser Query
parseSell = do
  _ <- parseLiteral "sell"
  _ <- parseChar '('
  wType <- parseWineType
  _ <- parseChar ','
  quantity <- parseQuantity
  _ <- parseChar ','
  price <- parsePrice
  _ <- parseChar ')'
  return $ Sell wType quantity price

-- <view> ::= "view" "(" ")"
parseView :: Parser Query
parseView = do
  _ <- parseLiteral "view"
  _ <- parseChar '('
  _ <- parseChar ')'
  return View

-- <wine_factory> ::= "wine_factory" "(" <task_list> ")"
parseWineFactory :: Parser Query
parseWineFactory = do
  _ <- parseLiteral "wine_factory"
  _ <- parseChar '('
  queryList <- parseTaskList
  _ <- parseChar ')'
  return $ WineFactory queryList

-- GrapeType Parser
parseGrapeType :: Parser GrapeType
parseGrapeType =
  (parseLiteral "CabernetSauvignon" >> return CabernetSauvignon)
    <|> (parseLiteral "Merlot" >> return Merlot)
    <|> (parseLiteral "PinotNoir" >> return PinotNoir)
    <|> (parseLiteral "Chardonnay" >> return Chardonnay)

-- WineType Parser
parseWineType :: Parser WineType
parseWineType =
  (parseLiteral "RedWine" >> return RedWine)
    <|> (parseLiteral "WhiteWine" >> return WhiteWine)
    <|> (parseLiteral "RoseWine" >> return RoseWine)

-- BarrelType Parser
parseBarrelType :: Parser BarrelType
parseBarrelType =
  (parseLiteral "Oak" >> return Oak)
    <|> (parseLiteral "Steel" >> return Steel)
    <|> (parseLiteral "Clay" >> return Clay)

-- Quantity Parser (Kg, L, Bottles)
parseQuantity :: Parser Quantity
parseQuantity = do
  amount <- parseInt
  unit <- parseString
  case unit of
    "kg" -> return $ Kg amount
    "l" -> return $ L amount
    "bottles" -> return $ Bottles amount
    _ -> empty

-- Duration Parser (Days, Months)
parseDuration :: Parser Duration
parseDuration = do
  num <- parseInt
  unit <- parseString
  case unit of
    "days" -> return (Days num)
    "months" -> return (Months num)
    _ -> empty

-- Price Parser
parsePrice :: Parser Double
parsePrice = do
  wholePart <- parseInt
  _ <- parseChar '.'
  fractionalPart <- parseInt
  return (read (show wholePart ++ "." ++ show fractionalPart))

-- Utility Parsers
sat :: (Char -> Bool) -> Parser Char
sat p = P $ \case
  [] -> Left "Empty String"
  s@(x : xs) -> if p x then Right (x, xs) else Left $ "Could not recognize: " ++ s

char :: Char -> Parser Char
char c = sat (== c)

parseChar :: Char -> Parser Char
parseChar = char

skipSpaces :: String -> String
skipSpaces = dropWhile (== ' ')

skipSpaces' :: Parser ()
skipSpaces' = P $ \input ->
  let input' = dropWhile (== ' ') input
   in Right ((), input')

parseLiteral :: String -> Parser String
parseLiteral [] = return []
parseLiteral (x : xs) = do
  _ <- skipSpaces'
  _ <- parseChar x
  parseLiteral xs

parseString :: Parser String
parseString = P $ \input ->
  let input' = skipSpaces input
   in if null input'
        then Right ("", "")
        else
          if head input' == '"'
            then parseQuotedString (tail input')
            else
              let (str, rest) = span (\c -> c /= ' ' && c /= ',' && c /= '(' && c /= ')') input'
               in Right (str, rest)
  where
    parseQuotedString [] = Left "Unexpected end of input in quoted string"
    parseQuotedString ('"' : rest) = Right ("", rest)
    parseQuotedString (x : rest) = case parseQuotedString rest of
      Right (str, rest') -> Right (x : str, rest')
      Left err -> Left err

parseInt :: Parser Int
parseInt = P $ \input ->
  let (digits, rest) = span isDigit (skipSpaces input)
   in if null digits
        then Left "Expected an integer"
        else Right (read digits, rest)