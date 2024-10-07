{-# LANGUAGE ImportQualifiedPost #-}
import Test.Tasty ( TestTree, defaultMain, testGroup )
import Test.Tasty.HUnit ( testCase, (@?=) )

import Lib2 qualified

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Lib2 tests"
  [ testCase "Harvest command parsing" $
      Lib2.parseQuery "harvest (CabernetSauvignon, 100, kg)" @?= 
        Right (Lib2.Harvest Lib2.CabernetSauvignon 100 Lib2.Kg),
        
    testCase "Ferment command parsing" $
      Lib2.parseQuery "ferment (Merlot, 30 days)" @?= 
        Right (Lib2.Ferment Lib2.Merlot (Lib2.Days 30)),
        
    testCase "Age command parsing" $
      Lib2.parseQuery "age (RedWine, 6 months, Oak)" @?= 
        Right (Lib2.Age Lib2.RedWine (Lib2.Months 6) Lib2.Oak),

    testCase "Bottle command parsing" $
      Lib2.parseQuery "bottle (WhiteWine, 50, bottles)" @?= 
        Right (Lib2.Bottle Lib2.WhiteWine 50 Lib2.Bottles),

    testCase "Sell command parsing" $
      Lib2.parseQuery "sell (RoseWine, 20, 15.99)" @?= 
        Right (Lib2.Sell Lib2.RoseWine 20 15.99),

    testCase "View command parsing" $
      Lib2.parseQuery "view" @?= Right Lib2.View,

    testCase "Invalid command parsing" $
      Lib2.parseQuery "invalid command" @?= Left "Unknown command",
        
    testCase "State transition with Harvest" $
      let initialState = Lib2.emptyState
      in case Lib2.parseQuery "harvest (CabernetSauvignon, 100, kg)" of
        Right query ->
            case Lib2.stateTransition initialState query of
                Right (_, newState) -> Lib2.processes newState @?= [query]
                Left err -> error err
        Left err -> error err,

    testCase "State transition with View" $
      let initialState = Lib2.emptyState
      in case Lib2.stateTransition initialState Lib2.View of
          Right (Just stateView, _) -> stateView @?= show initialState
          Left err -> error err,

    testCase "State transition with Bottle when there are enough grapes" $
      let initialState = Lib2.emptyState { Lib2.grapeInventory = [(Lib2.CabernetSauvignon, 100)] }
      in case Lib2.parseQuery "bottle (RedWine, 50, bottles)" of
        Right query ->
          case Lib2.stateTransition initialState query of
            Right (_, newState) -> 
              Lib2.wineInventory newState @?= [(Lib2.RedWine, 50)]
            Left err -> error err
        Left err -> error err,

    testCase "State transition with Age" $
      let initialState = Lib2.emptyState
      in case Lib2.parseQuery "age (RedWine, 6 months, Oak)" of
        Right query ->
          case Lib2.stateTransition initialState query of
            Right (_, newState) ->
              Lib2.processes newState @?= [query]
            Left err -> error err
        Left err -> error err,

    testCase "State transition with Sell when there is enough wine" $
      let initialState = Lib2.emptyState { Lib2.wineInventory = [(Lib2.RedWine, 100)] }
      in case Lib2.parseQuery "sell (RedWine, 50, 25.99)" of
        Right query ->
          case Lib2.stateTransition initialState query of
            Right (_, newState) ->
              Lib2.wineInventory newState @?= [(Lib2.RedWine, 50)]
            Left err -> error err
        Left err -> error err,

    testCase "State transition with Sell when there is not enough wine" $
      let initialState = Lib2.emptyState { Lib2.wineInventory = [(Lib2.RedWine, 10)] }
      in case Lib2.parseQuery "sell (RedWine, 50, 25.99)" of
        Right query ->
          case Lib2.stateTransition initialState query of
            Left err -> err @?= "Not enough wine to sell"
            Right _ -> error "Expected error for insufficient wine"
        Left err -> error err,

    testCase "State transition with Ferment when there are enough grapes" $
      let initialState = Lib2.emptyState { Lib2.grapeInventory = [(Lib2.Merlot, 50)] }
      in case Lib2.parseQuery "ferment (Merlot, 30 days)" of
        Right query ->
          case Lib2.stateTransition initialState query of
            Right (_, newState) ->
              Lib2.processes newState @?= [query]
            Left err -> error err
        Left err -> error err,

    testCase "State transition with Ferment when there are not enough grapes" $
      let initialState = Lib2.emptyState { Lib2.grapeInventory = [(Lib2.Merlot, 0)] }
      in case Lib2.parseQuery "ferment (Merlot, 30 days)" of
        Right query ->
          case Lib2.stateTransition initialState query of
            Left err -> err @?= "Not enough grapes to ferment"
            Right _ -> error "Expected error for insufficient grapes"
        Left err -> error err
  ]
