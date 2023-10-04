import qualified Test.Hash.Poseidon as Poseidon
-- import qualified Test.Lib.Array as Array
-- import qualified Test.Lib.W32 as W32
-- import qualified Test.BLAKE2s as BLAKE2s
-- import qualified Test.Util as Util
-- import qualified Test.SHA256 as SHA256
import Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Tests"
    [Poseidon.tests]
