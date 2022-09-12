import Test.Tasty
import qualified Test.Lib.Array as Array
import qualified Test.Lib.W32 as W32
import qualified Test.BLAKE2s as BLAKE2s
import qualified Test.Util as Util

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
    [Util.tests, Array.tests, W32.tests, BLAKE2s.tests]
