{-# LANGUAGE InstanceSigs #-}

module Lib3
  ( stateTransition,
    StorageOp (..),
    storageOpLoop,
    parseCommand,
    parseStatements,
    marshallState,
    renderStatements,
  )
where

import Control.Applicative (Alternative (many), (<|>))
import Control.Concurrent (Chan, newChan, readChan, writeChan)
import Control.Concurrent.STM (STM, TVar, atomically, readTVar, readTVarIO, writeTVar)
import Control.Monad (forever)
import Data.List (intercalate)
import Data.Maybe (fromJust, isNothing)
import qualified Lib2
import Parsers
import System.Directory (doesFileExist)

data StorageOp = Save String (Chan ()) | Load (Chan (Maybe String))

-- | This function is started from main
-- in a dedicated thread. It must be used to control
-- file access in a synchronized manner: read requests
-- from chan, do the IO operations needed and respond
-- to a channel provided in a request.
-- Modify as needed.
storageOpLoop :: Chan StorageOp -> IO ()
storageOpLoop opChan = forever $ do
  op <- readChan opChan
  case op of
    Save s chan -> do
      writeFile "state.txt" s
      writeChan chan ()
    Load chan -> do
      exists <- doesFileExist "state.txt"
      if exists
        then do
          s' <- readFile "state.txt"
          writeChan chan $ Just s'
        else writeChan chan Nothing

data Statements
  = Batch [Lib2.Query]
  | Single Lib2.Query
  deriving (Eq)

data Command
  = StatementCommand Statements
  | LoadCommand
  | SaveCommand
  deriving (Show, Eq)

instance Show Statements where
  show :: Statements -> String
  show (Single q) = show q
  show (Batch qs) = "BEGIN\n" ++ concatMap ((++ ";\n") . renderQuery) qs ++ "END\n"

-- | Parses user's input.
parseCommand :: String -> Either String (Command, String)
parseCommand = parse (StatementCommand <$> statements <|> parseLoad <|> parseSave)

parseLoad :: Parser Command
parseLoad = do
  _ <- parseLiteral "load"
  return LoadCommand

parseSave :: Parser Command
parseSave = do
  _ <- parseLiteral "save"
  return SaveCommand

-- | Parses Statement.
-- Must be used in parseCommand.
-- Reuse Lib2 as much as you can.
-- You can change Lib2.parseQuery signature if needed.
parseStatements :: String -> Either String (Statements, String)
parseStatements = parse statements

-- | Converts program's state into Statements
-- (probably a batch, but might be a single query)
marshallState :: Lib2.State -> Statements
marshallState state = Batch queries
  where
    -- Generate Harvest queries from grape inventory
    harvestQueries = map (\(gt, qty) -> Harvest gt (Kg qty)) (Lib2.grapeInventory state)

    -- Generate Ferment queries from grape inventory (assuming default durations for fermentation)
    fermentQueries = map (\(gt, _) -> Ferment gt (Days 30)) (Lib2.grapeInventory state) -- Example: Ferment all grapes for 30 days

    -- Generate Age queries for each wine type, assuming 12 months of aging in Oak barrels
    ageQueries = map (\(wt, _) -> Age wt (Months 12) Oak) (Lib2.wineInventory state)

    -- Generate Bottle queries for each wine type, assuming the wine needs to be bottled
    bottleQueries = map (\(wt, qty) -> Bottle wt (Bottles qty)) (Lib2.wineInventory state)

    -- Generate Sell queries for each wine type, assuming a fixed price (could be dynamic)
    sellQueries = map (\(wt, qty) -> Sell wt (Bottles qty) 15.99) (Lib2.wineInventory state) -- Example price: 15.99 per bottle

    -- Combine all queries into one batch
    queries = harvestQueries ++ fermentQueries ++ ageQueries ++ bottleQueries ++ sellQueries

renderQuery :: Query -> String
renderQuery (Harvest grapeType quantity) =
  "harvest(" ++ show grapeType ++ ", " ++ renderQuantity quantity ++ ")"
renderQuery (Ferment grapeType duration) =
  "ferment(" ++ show grapeType ++ ", " ++ renderDuration duration ++ ")"
renderQuery (Age wineType duration barrelType) =
  "age(" ++ show wineType ++ ", " ++ renderDuration duration ++ ", " ++ show barrelType ++ ")"
renderQuery (Bottle wineType quantity) =
  "bottle(" ++ show wineType ++ ", " ++ renderQuantity quantity ++ ")"
renderQuery (Sell wineType quantity price) =
  "sell(" ++ show wineType ++ ", " ++ renderQuantity quantity ++ ", " ++ show price ++ ")"
renderQuery View =
  "view()"
renderQuery (WineFactory queries) =
  "wine_factory(" ++ renderQueries queries ++ ")"

renderQuantity :: Lib2.Quantity -> String
renderQuantity (Lib2.Kg n) = show n ++ " kg"
renderQuantity (Lib2.L n) = show n ++ " l"
renderQuantity (Lib2.Bottles n) = show n ++ " bottles"

renderDuration :: Lib2.Duration -> String
renderDuration (Lib2.Days n) = show n ++ " days"
renderDuration (Lib2.Months n) = show n ++ " months"

-- Helper function to render a list of queries
renderQueries :: [Query] -> String
renderQueries = intercalate ", " . map renderQuery

-- | Renders Statements into a String which
-- can be parsed back into Statements by parseStatements
-- function. The String returned by this function must be used
-- as persist program's state in a file.
-- Must have a property test
-- for all s: parseStatements (renderStatements s) == Right(s, "")
renderStatements :: Statements -> String
renderStatements (Single q) = renderQuery q
renderStatements (Batch qs) = "BEGIN\n" ++ concatMap ((++ ";\n") . renderQuery) qs ++ "END\n"

-- | Updates a state according to a command.
-- Performs file IO via ioChan if needed.
-- This allows your program to share the state
-- between repl iterations, save the state to a file,
-- load the state from the file so the state is preserved
-- between program restarts.
-- Keep IO as small as possible.
-- State update must be executed atomically (STM).
-- Right contains an optional message to print, updated state
-- is stored in transactinal variable
stateTransition :: TVar Lib2.State -> Command -> Chan StorageOp -> IO (Either String (Maybe String))
stateTransition s SaveCommand ioChan = do
  s' <- readTVarIO s
  chan <- newChan :: IO (Chan ())
  writeChan ioChan (Save (renderStatements $ marshallState s') chan)
  readChan chan
  return $ Right $ Just "State saved successfully"
stateTransition s LoadCommand ioChan = do
  chan <- newChan :: IO (Chan (Maybe String))
  writeChan ioChan (Load chan)
  qs <- readChan chan
  if isNothing qs
    then return (Left "No state file found")
    else case parseStatements $ fromJust qs of
      Left e -> do
        return $ Left $ "Failed to load state from file:\n" ++ e
      Right (qs', _) -> atomically $ atomicStatements s qs'
stateTransition s (StatementCommand sts) _ = atomically $ atomicStatements s sts

transitionThroughList :: Lib2.State -> [Lib2.Query] -> Either String (Maybe String, Lib2.State)
transitionThroughList _ [] = Left "Empty query list"
transitionThroughList s (q : qs) = case Lib2.stateTransition s q of
  Left e -> Left e
  Right (msg, ns) ->
    if null qs
      then Right (msg, ns)
      else case transitionThroughList ns qs of
        Left e -> Left e
        Right (msg', ns') -> Right ((\x y -> x ++ "\n" ++ y) <$> msg <*> msg', ns')

atomicStatements :: TVar Lib2.State -> Statements -> STM (Either String (Maybe String))
atomicStatements s (Batch qs) = do
  s' <- readTVar s
  case transitionThroughList s' qs of
    Left e -> return $ Left e
    Right (msg, ns) -> do
      writeTVar s ns
      return $ Right msg
atomicStatements s (Single q) = do
  s' <- readTVar s
  case Lib2.stateTransition s' q of
    Left e -> return $ Left e
    Right (msg, ns) -> do
      writeTVar s ns
      return $ Right msg

statements :: Parser Statements
statements =
  ( do
      _ <- parseLiteral "BEGIN"
      _ <- parseLiteral "\n"
      q <-
        many
          ( do
              q <- Lib2.parseTask
              _ <- parseLiteral ";"
              _ <- parseLiteral "\n"
              return q
          )
      _ <- parseLiteral "END"
      _ <- parseLiteral "\n"
      return $ Batch q
  )
    <|> (Single <$> Lib2.parseTask)