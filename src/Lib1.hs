module Lib1
    ( completions
    ) where

-- | This function returns a list of words
-- to be autocompleted in your program's repl.
completions :: [String]
completions =
  [ -- Processes
    "harvest"
  , "ferment"
  , "age"
  , "bottle"
  , "sell"

    -- Grape types
  , "CabernetSauvignon"
  , "Merlot"
  , "PinotNoir"
  , "Chardonnay"

    -- Wine types
  , "RedWine"
  , "WhiteWine"
  , "RoseWine"

    -- Barrel types
  , "Oak"
  , "Steel"
  , "Clay"

    -- Units
  , "kg"
  , "L"
  , "bottles"

    -- Duration
  , "days"
  , "months"
  ]
