{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Use lambda-case" #-}
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
    Parser,
    parse,
  )
where

import Control.Applicative (Alternative (empty), optional, (<|>))
import Control.Monad.Except (throwError)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except
import Control.Monad.Trans.State
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

type Parser a = ExceptT String (State String) a

parse :: Parser a -> String -> (Either String a, String)
parse parser = runState (runExceptT parser)

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
sat p = do
  input <- lift get
  case input of
    [] -> throwError "Empty String"
    (x : xs) ->
      if p x
        then lift (put xs) >> return x
        else throwError $ "Could not recognize: " ++ [x]

char :: Char -> Parser Char
char c = sat (== c)

parseChar :: Char -> Parser Char
parseChar = char

skipSpaces :: String -> String
skipSpaces = dropWhile (== ' ')

skipSpaces' :: Parser ()
skipSpaces' = lift (modify (dropWhile (== ' ')))

parseLiteral :: String -> Parser String
parseLiteral [] = return []
parseLiteral (x : xs) = do
  skipSpaces'
  _ <- parseChar x
  parseLiteral xs

parseString :: Parser String
parseString = do
  input <- lift get
  let input' = skipSpaces input
  if null input'
    then return ""
    else
      if head input' == '"'
        then parseQuotedString (tail input')
        else
          let (str, rest) = span (\c -> c /= ' ' && c /= ',' && c /= '(' && c /= ')') input'
           in lift (put rest) >> return str
  where
    parseQuotedString [] = throwError "Unexpected end of input in quoted string"
    parseQuotedString ('"' : rest) = lift (put rest) >> return ""
    parseQuotedString (x : rest) = do
      str <- parseQuotedString rest
      return (x : str)

parseInt :: Parser Int
parseInt = do
  input <- lift get
  let (digits, rest) = span isDigit (skipSpaces input)
  if null digits
    then throwError "Expected an integer"
    else do
      lift (put rest)
      return (read digits)