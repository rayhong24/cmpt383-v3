module Main ( main ) where

import qualified SwitchableStackTests

import TestingFramework

allTests :: TestSuite
allTests = SwitchableStackTests.allTests

main :: IO ()
main = do
    v1 <- runTests allTests
    v2 <- runTestsFeedback allTests
    putStrLn v1
    putStrLn "\n"
    putStrLn v2