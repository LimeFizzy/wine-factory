{-# LANGUAGE DeriveFunctor #-}

module Main (main) where

import Control.Lens hiding (view)
import Control.Monad.Free (Free (..), liftF)
import Control.Monad.State
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.String.Conversions (cs)
import Network.Wreq hiding (get)
import System.Environment (getArgs)

-- Command Data Type
data Command next
  = Harvest String String next -- GrapeType, Quantity
  | Ferment String String next -- GrapeType, Duration
  | Age String String String next -- WineType, Duration, BarrelType
  | Bottle String String next -- WineType, Quantity
  | Sell String String Double next -- WineType, Quantity, Price
  | View (String -> next) -- View the current state
  deriving (Functor)

type WineDSL = Free Command

-- Smart Constructors
harvest :: String -> String -> WineDSL ()
harvest grapeType quantity = liftF $ Harvest grapeType quantity ()

ferment :: String -> String -> WineDSL ()
ferment grapeType duration = liftF $ Ferment grapeType duration ()

age :: String -> String -> String -> WineDSL ()
age wineType duration barrelType = liftF $ Age wineType duration barrelType ()

bottle :: String -> String -> WineDSL ()
bottle wineType quantity = liftF $ Bottle wineType quantity ()

sell :: String -> String -> Double -> WineDSL ()
sell wineType quantity price = liftF $ Sell wineType quantity price ()

view :: WineDSL String
view = liftF $ View id

-- HTTP Request per Command
runHttpSingle :: WineDSL a -> IO a
runHttpSingle (Pure a) = return a
runHttpSingle (Free (Harvest grapeType quantity next)) = do
  putStrLn $ "Sending request: harvest(" ++ grapeType ++ ", " ++ quantity ++ ")"
  _ <- post "http://localhost:3000" (cs $ "harvest(" ++ grapeType ++ ", " ++ quantity ++ ")" :: ByteString)
  runHttpSingle next
runHttpSingle (Free (Ferment grapeType duration next)) = do
  putStrLn $ "Sending request: ferment(" ++ grapeType ++ ", " ++ duration ++ ")"
  _ <- post "http://localhost:3000" (cs $ "ferment(" ++ grapeType ++ ", " ++ duration ++ ")" :: ByteString)
  runHttpSingle next
runHttpSingle (Free (Age wineType duration barrelType next)) = do
  putStrLn $ "Sending request: age(" ++ wineType ++ ", " ++ duration ++ ", " ++ barrelType ++ ")"
  _ <- post "http://localhost:3000" (cs $ "age(" ++ wineType ++ ", " ++ duration ++ ", " ++ barrelType ++ ")" :: ByteString)
  runHttpSingle next
runHttpSingle (Free (Bottle wineType quantity next)) = do
  putStrLn $ "Sending request: bottle(" ++ wineType ++ ", " ++ quantity ++ ")"
  _ <- post "http://localhost:3000" (cs $ "bottle(" ++ wineType ++ ", " ++ quantity ++ ")" :: ByteString)
  runHttpSingle next
runHttpSingle (Free (Sell wineType quantity price next)) = do
  putStrLn $ "Sending request: sell(" ++ wineType ++ ", " ++ quantity ++ ", " ++ show price ++ ")"
  _ <- post "http://localhost:3000" (cs $ "sell(" ++ wineType ++ ", " ++ quantity ++ ", " ++ show price ++ ")" :: ByteString)
  runHttpSingle next
runHttpSingle (Free (View next)) = do
  putStrLn "Sending request: view()"
  resp <- post "http://localhost:3000" (cs "view()" :: ByteString)
  runHttpSingle (next $ cs $ resp ^. responseBody)

-- Smart HTTP Batcher
runHttpBatch :: WineDSL a -> IO a
runHttpBatch = runHttpBatch' []

runHttpBatch' :: [String] -> WineDSL a -> IO a
runHttpBatch' acc (Pure a) = do
  unless (null acc) $ do
    putStrLn $ "Sending batch request: " ++ unlines acc
    _ <- post "http://localhost:3000" (cs $ unlines acc :: ByteString)
    return ()
  return a
runHttpBatch' acc (Free (Harvest grapeType quantity next)) =
  runHttpBatch' (acc ++ ["harvest(" ++ grapeType ++ ", " ++ quantity ++ ")"]) next
runHttpBatch' acc (Free (Ferment grapeType duration next)) =
  runHttpBatch' (acc ++ ["ferment(" ++ grapeType ++ ", " ++ duration ++ ")"]) next
runHttpBatch' acc (Free (Age wineType duration barrelType next)) =
  runHttpBatch' (acc ++ ["age(" ++ wineType ++ ", " ++ duration ++ ", " ++ barrelType ++ ")"]) next
runHttpBatch' acc (Free (Bottle wineType quantity next)) =
  runHttpBatch' (acc ++ ["bottle(" ++ wineType ++ ", " ++ quantity ++ ")"]) next
runHttpBatch' acc (Free (Sell wineType quantity price next)) =
  runHttpBatch' (acc ++ ["sell(" ++ wineType ++ ", " ++ quantity ++ ", " ++ show price ++ ")"]) next
runHttpBatch' acc (Free (View next)) = do
  unless (null acc) $ do
    putStrLn $ "Sending batch request: " ++ unlines acc
    _ <- post "http://localhost:3000" (cs $ unlines acc :: ByteString)
    return ()
  resp <- post "http://localhost:3000" (cs "view()" :: ByteString)
  runHttpBatch' [] (next $ cs $ resp ^. responseBody)

-- In-Memory Interpreter
type InMemoryState = [(String, String)]

runInMemory :: WineDSL a -> State InMemoryState a
runInMemory (Pure a) = return a
runInMemory (Free (Harvest grapeType quantity next)) = do
  modify ((grapeType, quantity) :)
  runInMemory next
runInMemory (Free (Ferment _ _ next)) = runInMemory next
runInMemory (Free (Age _ _ _ next)) = runInMemory next
runInMemory (Free (Bottle wineType quantity next)) = do
  modify ((wineType, quantity) :)
  runInMemory next
runInMemory (Free (Sell wineType quantity _ next)) = do
  modify (filter (\(wt, qt) -> not (wt == wineType && qt == quantity)))
  runInMemory next
runInMemory (Free (View next)) = do
  currentState <- Control.Monad.State.get
  runInMemory (next $ show currentState)

-- Main Program
main :: IO ()
main = do
  args <- getArgs
  let program = do
        harvest "CabernetSauvignon" "100kg"
        ferment "CabernetSauvignon" "7 days"
        age "RedWine" "12 months" "Oak"
        bottle "RedWine" "500 bottles"
        sell "RedWine" "100 bottles" 25.0
        view

  case args of
    ["single"] -> do
      putStrLn "Running with HTTP single request per command:"
      _ <- runHttpSingle program
      return ()
    ["batch"] -> do
      putStrLn "Running with HTTP batch requests:"
      _ <- runHttpBatch program
      return ()
    ["memory"] -> do
      putStrLn "Running with in-memory interpreter for testing:"
      let (result, finalState) = runState (runInMemory program) []
      print result
      print finalState
    _ -> putStrLn "Usage: stack exec wine-factory-client [single|batch|memory]"
