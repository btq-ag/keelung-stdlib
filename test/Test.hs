import qualified Test.Hash.Poseidon as Poseidon
import qualified Test.Lib.Array as Array
import Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Tests"
    [ Array.tests,
      Poseidon.tests
    ]
