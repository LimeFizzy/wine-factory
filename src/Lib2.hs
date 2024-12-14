{-# LANGUAGE InstanceSigs #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use fromMaybe" #-}

module Lib2
  ( Query (..),
    GrapeType (..),
    WineType (..),
    Quantity (..),
    BarrelType (..),
    Duration (..),
    parseTask,
    parseTaskList,
    parseQuery,
    State (..),
    emptyState,
    stateTransition,
  )
where

import Parsers

data State = State
  { grapeInventory :: [(GrapeType, Int)], -- Tracks harvested grapes in kg.
    wineInventory :: [(WineType, Int)] -- Tracks bottled wine in quantity.
  }
  deriving (Eq, Show)

-- | Initial program state.
emptyState :: State
emptyState =
  State
    { grapeInventory = [],
      wineInventory = []
    }

parseQuery :: String -> Either String Query
parseQuery s =
  case parse parseTaskList s of
    (Left e, _) -> Left e
    (Right qs, r) ->
      if null r
        then case qs of
          [q] -> Right q
          _ -> Right (WineFactory qs)
        else Left ("Unrecognized characters: " ++ r)

stateTransition :: State -> Query -> Either String (Maybe String, State)
stateTransition st query = case query of
  Harvest grapeType (Kg amount) ->
    let newInventory = addToInventory grapeType amount (grapeInventory st)
        newState = st {grapeInventory = newInventory}
     in Right (Just $ "Harvested " ++ show amount ++ " kg of " ++ show grapeType, newState)
  Ferment grapeType duration ->
    if lookupInventory grapeType (grapeInventory st) > 0
      then Right (Just $ "Started fermentation of " ++ show grapeType ++ " for " ++ show duration, st)
      else Left $ "Not enough " ++ show grapeType ++ " to ferment."
  Age wineType duration barrelType ->
    if lookupInventory wineType (wineInventory st) > 0
      then Right (Just $ "Aging " ++ show wineType ++ " for " ++ show duration ++ " in " ++ show barrelType, st)
      else Left $ "Not enough " ++ show wineType ++ " to age."
  Bottle wineType (Bottles quantity) ->
    let newInventory = addToInventory wineType quantity (wineInventory st)
        newState = st {wineInventory = newInventory}
     in Right (Just $ "Bottled " ++ show quantity ++ " of " ++ show wineType, newState)
  Sell wineType (Bottles quantity) price ->
    let currentQuantity = lookupInventory wineType (wineInventory st)
     in if currentQuantity >= quantity
          then
            let newInventory = removeFromInventory wineType quantity (wineInventory st)
                newState = st {wineInventory = newInventory}
             in Right (Just $ "Sold " ++ show quantity ++ " bottles of " ++ show wineType ++ " for $" ++ show price, newState)
          else Left "Not enough bottles to sell."
  View ->
    let grapeView = unlines [show g ++ ": " ++ show q ++ " kg" | (g, q) <- grapeInventory st]
        wineView = unlines [show w ++ ": " ++ show q ++ " bottles" | (w, q) <- wineInventory st]
     in Right (Just $ "Grape Inventory:\n" ++ grapeView ++ "Wine Inventory:\n" ++ wineView, st)
  WineFactory queryList ->
    foldl processQuery (Right (Just "", st)) queryList
    where
      processQuery (Left err) _ = Left err
      processQuery (Right (msg, currState)) nextQuery =
        case stateTransition currState nextQuery of
          Left err -> Left err
          Right (nextMsg, newState) -> Right (combineMessages msg nextMsg, newState)

-- | Helper functions for inventory management.
addToInventory :: (Eq a) => a -> Int -> [(a, Int)] -> [(a, Int)]
addToInventory item quantity [] = [(item, quantity)]
addToInventory item quantity ((i, q) : xs)
  | i == item = (i, q + quantity) : xs
  | otherwise = (i, q) : addToInventory item quantity xs

removeFromInventory :: (Eq a) => a -> Int -> [(a, Int)] -> [(a, Int)]
removeFromInventory _ _ [] = []
removeFromInventory item quantity ((i, q) : xs)
  | i == item = if q > quantity then (i, q - quantity) : xs else xs
  | otherwise = (i, q) : removeFromInventory item quantity xs

-- | Helper functions for state transitions.
combineMessages :: Maybe String -> Maybe String -> Maybe String
combineMessages Nothing Nothing = Nothing
combineMessages (Just msg) Nothing = Just msg
combineMessages Nothing (Just msg) = Just msg
combineMessages (Just msg1) (Just msg2) = Just (msg1 ++ "\n" ++ msg2)

lookupInventory :: (Eq a) => a -> [(a, Int)] -> Int
lookupInventory item = maybe 0 id . lookup item