import Test.Tasty
import qualified Test.Lib.Array as Array

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
    [Array.tests]
