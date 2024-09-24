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
        
    testCase "Invalid command parsing" $
      Lib2.parseQuery "invalid command" @?= Left "Unknown command",
        
    testCase "State transition with Harvest" $
      let initialState = Lib2.emptyState
      in case Lib2.parseQuery "harvest (CabernetSauvignon, 100, kg)" of
        Right query ->
            case Lib2.stateTransition initialState query of
                Right (_, newState) -> Lib2.processes newState @?= [query]
                Left err -> error err
        Left err -> error err
  ]
