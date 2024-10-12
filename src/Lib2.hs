{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant lambda" #-}

module Lib2
  ( Query (..),
    GrapeType (..),
    WineType (..),
    Quantity (..),
    BarrelType (..),
    Duration (..),
    parseQuery,
    State (..),
    emptyState,
    stateTransition,
  )
where

import Data.Char (isDigit)

-- | Data types for Queries and State
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

data State = State
  { grapeInventory :: [(GrapeType, Int)], -- Tracks harvested grapes in kg.
    wineInventory :: [(WineType, Int)] -- Tracks bottled wine in quantity.
  }
  deriving (Eq, Show)

type Parser a = String -> Either String (a, String)

and3' ::
  (a -> b -> c -> d) ->
  Parser a ->
  Parser b ->
  Parser c ->
  Parser d
and3' comb p1 p2 p3 = \input ->
  case p1 input of
    Right (v1, r1) ->
      case p2 r1 of
        Right (v2, r2) ->
          case p3 r2 of
            Right (v3, r3) -> Right (comb v1 v2 v3, r3)
            Left e3 -> Left e3
        Left e2 -> Left e2
    Left e1 -> Left e1

and4' :: (a -> b -> c -> d -> e) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e
and4' comb p1 p2 p3 p4 = \input ->
  case p1 input of
    Right (v1, r1) ->
      case p2 r1 of
        Right (v2, r2) ->
          case p3 r2 of
            Right (v3, r3) ->
              case p4 r3 of
                Right (v4, r4) -> Right (comb v1 v2 v3 v4, r4)
                Left e4 -> Left e4
            Left e3 -> Left e3
        Left e2 -> Left e2
    Left e1 -> Left e1

and6' :: (a -> b -> c -> d -> e -> f -> g) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser f -> Parser g
and6' comb p1 p2 p3 p4 p5 p6 = \input ->
  case p1 input of
    Right (v1, r1) ->
      case p2 r1 of
        Right (v2, r2) ->
          case p3 r2 of
            Right (v3, r3) ->
              case p4 r3 of
                Right (v4, r4) ->
                  case p5 r4 of
                    Right (v5, r5) ->
                      case p6 r5 of
                        Right (v6, r6) -> Right (comb v1 v2 v3 v4 v5 v6, r6)
                        Left e6 -> Left e6
                    Left e5 -> Left e5
                Left e4 -> Left e4
            Left e3 -> Left e3
        Left e2 -> Left e2
    Left e1 -> Left e1

and8' ::
  (a -> b -> c -> d -> e -> f -> g -> h -> i) ->
  Parser a ->
  Parser b ->
  Parser c ->
  Parser d ->
  Parser e ->
  Parser f ->
  Parser g ->
  Parser h ->
  Parser i
and8' comb p1 p2 p3 p4 p5 p6 p7 p8 = \input ->
  case p1 input of
    Right (v1, r1) ->
      case p2 r1 of
        Right (v2, r2) ->
          case p3 r2 of
            Right (v3, r3) ->
              case p4 r3 of
                Right (v4, r4) ->
                  case p5 r4 of
                    Right (v5, r5) ->
                      case p6 r5 of
                        Right (v6, r6) ->
                          case p7 r6 of
                            Right (v7, r7) ->
                              case p8 r7 of
                                Right (v8, r8) ->
                                  Right (comb v1 v2 v3 v4 v5 v6 v7 v8, r8)
                                Left e8 -> Left e8
                            Left e7 -> Left e7
                        Left e6 -> Left e6
                    Left e5 -> Left e5
                Left e4 -> Left e4
            Left e3 -> Left e3
        Left e2 -> Left e2
    Left e1 -> Left e1

or7' :: Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a
or7' p1 p2 p3 p4 p5 p6 p7 = \input ->
  case p1 input of
    Right r1 -> Right r1
    Left e1 -> case p2 input of
      Right r2 -> Right r2
      Left e2 -> case p3 input of
        Right r3 -> Right r3
        Left e3 -> case p4 input of
          Right r4 -> Right r4
          Left e4 -> case p5 input of
            Right r5 -> Right r5
            Left e5 -> case p6 input of
              Right r6 -> Right r6
              Left e6 -> case p7 input of
                Right r7 -> Right r7
                Left e7 -> Left (e1 ++ "; " ++ e2 ++ "; " ++ e3 ++ "; " ++ e4 ++ "; " ++ e5 ++ "; " ++ e6 ++ "; " ++ e7)

skipSpaces :: String -> String
skipSpaces = dropWhile (== ' ')

parseChar :: Char -> Parser Char
parseChar _ [] = Left "Unexpected end of input"
parseChar c input =
  let input' = skipSpaces input
   in if null input'
        then Left "Unexpected end of input"
        else
          if head input' == c
            then Right (c, tail input')
            else Left $ "Expected '" ++ [c] ++ "', but found '" ++ [head input'] ++ "'"

parseLiteral :: String -> Parser String
parseLiteral [] input = Right ([], input)
parseLiteral (x : xs) input =
  let input' = skipSpaces input
   in if null input'
        then Left "Unexpected end of input"
        else
          if head input' == x
            then case parseLiteral xs (tail input') of
              Right (str, rest) -> Right (x : str, rest)
              Left err -> Left err
            else Left $ "Expected \"" ++ (x : xs) ++ "\", but found \"" ++ take (length (x : xs)) input' ++ "\""

parseString :: Parser String
parseString input =
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
parseInt input =
  let (digits, rest) = span isDigit (skipSpaces input)
   in if null digits
        then Left "Expected an integer"
        else Right (read digits, rest)

parseDouble :: Parser Double
parseDouble input =
  let (digits, rest) = span (\c -> isDigit c || c == '.') (skipSpaces input)
   in if null digits
        then Left "Expected a double"
        else Right (read digits, rest)

-- | Parse grape type
-- <grape_type> ::= "CabernetSauvignon" | "Merlot" | "PinotNoir" | "Chardonnay"
parseGrapeType :: Parser GrapeType
parseGrapeType input = case parseString (skipSpaces input) of
  Right ("CabernetSauvignon", rest) -> Right (CabernetSauvignon, rest)
  Right ("Merlot", rest) -> Right (Merlot, rest)
  Right ("PinotNoir", rest) -> Right (PinotNoir, rest)
  Right ("Chardonnay", rest) -> Right (Chardonnay, rest)
  _ -> Left "Failed to parse grape type"

-- | Parse wine type
-- <wine_type> ::= "RedWine" | "WhiteWine" | "RoseWine"
parseWineType :: Parser WineType
parseWineType input = case parseString (skipSpaces input) of
  Right ("RedWine", rest) -> Right (RedWine, rest)
  Right ("WhiteWine", rest) -> Right (WhiteWine, rest)
  Right ("RoseWine", rest) -> Right (RoseWine, rest)
  _ -> Left "Failed to parse wine type"

-- | Parse unit
-- <unit> ::= "kg" | "L" | "bottles"
parseQuantityUnit :: Parser (Int -> Quantity)
parseQuantityUnit input = case parseString (skipSpaces input) of
  Right ("kg", rest) -> Right (Kg, rest)
  Right ("L", rest) -> Right (L, rest)
  Right ("bottles", rest) -> Right (Bottles, rest)
  _ -> Left "Failed to parse unit"

parseQuantity :: Parser Quantity
parseQuantity input = case parseInt input of
  Right (num, rest) -> case parseQuantityUnit rest of
    Right (unit, rest') -> Right (unit num, rest')
    Left err -> Left err
  Left err -> Left err

-- | Parse barrel type
-- <barrel_type> ::= "Oak" | "Steel" | "Clay"
parseBarrelType :: Parser BarrelType
parseBarrelType input = case parseString (skipSpaces input) of
  Right ("Oak", rest) -> Right (Oak, rest)
  Right ("Steel", rest) -> Right (Steel, rest)
  Right ("Clay", rest) -> Right (Clay, rest)
  _ -> Left "Failed to parse barrel type"

parseDurationUnit :: Parser (Int -> Duration)
parseDurationUnit input = case parseString (skipSpaces input) of
  Right ("days", rest) -> Right (Days, rest)
  Right ("months", rest) -> Right (Months, rest)
  _ -> Left "Failed to parse duration unit"

-- | Parse duration
-- <duration> ::= <number> "days" | <number> "months"
parseDuration :: Parser Duration
parseDuration input = case parseInt input of
  Right (num, rest) -> case parseDurationUnit rest of
    Right (unit, rest') -> Right (unit num, rest')
    Left err -> Left err
  Left err -> Left err

parseHarvest :: Parser Query
parseHarvest =
  and6'
    (\_ _ gType _ quantity _ -> Harvest gType quantity)
    (parseLiteral "harvest")
    (parseChar '(')
    parseGrapeType
    (parseChar ',')
    parseQuantity
    (parseChar ')')

parseFerment :: Parser Query
parseFerment =
  and6'
    (\_ _ gtype _ duration _ -> Ferment gtype duration)
    (parseLiteral "ferment")
    (parseChar '(')
    parseGrapeType
    (parseChar ',')
    parseDuration
    (parseChar ')')

parseBottle :: Parser Query
parseBottle =
  and6'
    (\_ _ wType _ quantity _ -> Bottle wType quantity)
    (parseLiteral "bottle")
    (parseChar '(')
    parseWineType
    (parseChar ',')
    parseQuantity
    (parseChar ')')

parseAge :: Parser Query
parseAge =
  and8'
    (\_ _ wType _ duration _ bType _ -> Age wType duration bType)
    (parseLiteral "age")
    (parseChar '(')
    parseWineType
    (parseChar ',')
    parseDuration
    (parseChar ',')
    parseBarrelType
    (parseChar ')')

parseSell :: Parser Query
parseSell =
  and8'
    (\_ _ wType _ quantity _ price _ -> Sell wType quantity price)
    (parseLiteral "sell")
    (parseChar '(')
    parseWineType
    (parseChar ',')
    parseQuantity
    (parseChar ',')
    parseDouble
    (parseChar ')')

-- | Parse view command
parseView :: Parser Query
parseView =
  and3'
    (\_ _ _ -> View)
    (parseLiteral "view")
    (parseChar '(')
    (parseChar ')')

parseProcess :: Parser Query
parseProcess = or7' parseHarvest parseFerment parseAge parseBottle parseSell parseView parseWineFactory

parseProcessList :: Parser [Query]
parseProcessList input = case parseProcess input of
  Right (firstQuery, rest) ->
    case parseChar ',' rest of
      Right (_, afterComma) -> case parseProcessList afterComma of
        Right (otherQueries, finalRest) -> Right (firstQuery : otherQueries, finalRest)
        Left err -> Left err
      Left _ -> Right ([firstQuery], rest)
  Left err -> Left err

parseWineFactory :: Parser Query
parseWineFactory =
  and4'
    (\_ _ queryList _ -> WineFactory queryList)
    (parseLiteral "wine_factory")
    (parseChar '(')
    parseProcessList
    (parseChar ')')

-- | Parses user's input.
parseQuery :: String -> Either String Query
parseQuery input = case or7' parseHarvest parseFerment parseBottle parseAge parseSell parseView parseWineFactory input of
  Right (query, _) -> Right query
  _ -> Left "Failed to parse query: Unknown command"

-- | Initial program state.
emptyState :: State
emptyState =
  State
    { grapeInventory = [],
      wineInventory = []
    }

-- | Transition the state with a new query.
stateTransition :: State -> Query -> Either String (Maybe String, State)
stateTransition st query = case query of
  Harvest grapeType quantity ->
    if getQuantity quantity <= 0
      then Left "Harvest quantity must be greater than zero."
      else
        let updatedInventory = addToInventory grapeType (getQuantity quantity) (grapeInventory st)
            newState = st {grapeInventory = updatedInventory}
         in Right (Just ("Harvested " ++ show (getQuantity quantity) ++ " kg of " ++ show grapeType), newState)
  Ferment grapeType duration ->
    let currentGrapes = lookupInventory grapeType (grapeInventory st)
     in if currentGrapes > 0
          then Right (Just ("Started fermentation of " ++ show grapeType ++ " for " ++ show duration), st)
          else Left ("Not enough " ++ show grapeType ++ " to ferment.")
  Age wineType duration barrelType ->
    let currentWineQuantity = lookupInventory wineType (wineInventory st)
     in if currentWineQuantity > 0
          then Right (Just ("Aging " ++ show wineType ++ " for " ++ show duration ++ " in " ++ show barrelType), st)
          else Left ("Not enough " ++ show wineType ++ " to age.")
  Bottle wineType quantity ->
    if getBottleQuantity quantity <= 0
      then Left "Bottling quantity must be greater than zero."
      else
        let updatedInventory = addToInventory wineType (getBottleQuantity quantity) (wineInventory st)
            newState = st {wineInventory = updatedInventory}
         in Right (Just ("Bottled " ++ show (getBottleQuantity quantity) ++ " of " ++ show wineType), newState)
  Sell wineType quantity price ->
    let currentWineQuantity = lookupInventory wineType (wineInventory st)
     in if currentWineQuantity >= getBottleQuantity quantity
          then
            let newWineInventory = removeFromInventory wineType (getBottleQuantity quantity) (wineInventory st)
                newState = st {wineInventory = newWineInventory}
             in Right (Just ("Sold " ++ show (getBottleQuantity quantity) ++ " of " ++ show wineType ++ " for $" ++ show price), newState)
          else Left "Insufficient inventory to sell the requested quantity."
  View ->
    let grapeList = unlines $ map (\(gType, qty) -> show qty ++ " kg of " ++ show gType) (grapeInventory st)
        wineList = unlines $ map (\(wType, qty) -> show qty ++ " bottles of " ++ show wType) (wineInventory st)
     in Right (Just ("Grape Inventory:\n" ++ grapeList ++ "\nWine Inventory:\n" ++ wineList), st)
  WineFactory queryList ->
    -- Process each query in the list and accumulate the result
    foldl processQuery (Right (Just "", st)) queryList
    where
      -- Helper function to process each query and transition the state
      processQuery :: Either String (Maybe String, State) -> Query -> Either String (Maybe String, State)
      processQuery (Left err) _ = Left err -- Stop if an error occurred
      processQuery (Right (_, currentState)) nextQuery =
        case stateTransition currentState nextQuery of
          Left err -> Left err
          Right (Just result, newState) ->
            Right (Just (result ++ "\nProcessed: " ++ show nextQuery), newState)
          Right (Nothing, newState) -> Right (Nothing, newState) -- No output, just transition state

-- Helper function to get quantity from Quantity type
getQuantity :: Quantity -> Int
getQuantity (Kg q) = q
getQuantity (L q) = q
getQuantity (Bottles q) = q

-- Helper function to get quantity from Bottles type
getBottleQuantity :: Quantity -> Int
getBottleQuantity (Bottles q) = q
getBottleQuantity _ = 0 -- Assuming that only bottles are relevant for this function

-- Helper function to add to inventory
addToInventory :: (Eq a) => a -> Int -> [(a, Int)] -> [(a, Int)]
addToInventory item quantity [] = [(item, quantity)]
addToInventory item quantity ((i, q) : xs)
  | i == item = (i, q + quantity) : xs
  | otherwise = (i, q) : addToInventory item quantity xs

-- Helper function to remove from inventory
removeFromInventory :: (Eq a) => a -> Int -> [(a, Int)] -> [(a, Int)]
removeFromInventory _ _ [] = []
removeFromInventory item quantity ((i, q) : xs)
  | i == item = if q > quantity then (i, q - quantity) : xs else xs
  | otherwise = (i, q) : removeFromInventory item quantity xs

-- Helper function to lookup inventory quantity
lookupInventory :: (Eq a) => a -> [(a, Int)] -> Int
lookupInventory _ [] = 0
lookupInventory item ((i, q) : xs)
  | i == item = q
  | otherwise = lookupInventory item xs
