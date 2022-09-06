import Test.Tasty
import qualified Test.Lib.ArrayI as ArrayI

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
    [ArrayI.tests]
