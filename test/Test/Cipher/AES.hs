{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Cipher.AES (run, tests) where

import Cipher.AES qualified as AES
import Data.Functor
import Keelung
import Keelung.Constraint.R1CS qualified as R1CS
import Test.Tasty
import Test.Tasty.HUnit

run :: IO ()
run = defaultMain tests

-- tests :: SpecWith ()
-- tests = describe "Interval Sets" $ do

tests :: TestTree
tests =
  testGroup
    "AES"
    [ testGroup
        "Constraint Count"
        [ testCase "AES 128" $ testAES128Count 21984
        ],
      testGroup
        "inversePK"
        [ testCase "0x00" $ testInversePK 0x00 0x00
        ],
      testGroup
        "SubByte"
        [ testCase "0x00" $ testSubByte 0x00 0x63,
          testCase "0x01" $ testSubByte 0x01 0x7c,
          testCase "0x02" $ testSubByte 0x02 0x77,
          testCase "0x03" $ testSubByte 0x03 0x7b,
          testCase "0x53" $ testSubByte 0x53 0xed
        ],
      testGroup
        "SubByte Count"
        [ testCase "sBox" $ testSubByteCount1 (26 + 24)
        -- ,
        --   testCase "sBox2" $ testSubByteCount2 (8 + 111),
        --   testCase "sBox3" $ testSubByteCount3 (8 + 247)
        ]
    ]
  where
    testInversePK :: Integer -> Integer -> Assertion
    testInversePK inputs expected = do
      actual <- solveOutput AES.pkField (input Public >>= AES.inversePK) [inputs] []
      actual @?= [expected]

    testSubByte :: Integer -> Integer -> Assertion
    testSubByte inputs expected = do
      -- actual <- interpret AES.nistField (AES.sBox2 (fromInteger inputs)) [] []
      actual <- interpret AES.nistField (input Public >>= AES.sBox) [inputs] []
      -- actual <- interpret AES.nistField (return $ AES.sBox3 (fromInteger inputs)) [] []
      actual @?= [expected]

    testSubByteCount1 :: Int -> Assertion
    testSubByteCount1 expected = do
      actual <- numberOfConstraints AES.nistField (input Public >>= AES.sBox)
      actual @?= Just expected

    testSubByteCount2 :: Int -> Assertion
    testSubByteCount2 expected = do
      actual <- numberOfConstraints AES.nistField (input Public >>= AES.sBox2)
      actual @?= Just expected

    testSubByteCount3 :: Int -> Assertion
    testSubByteCount3 expected = do
      actual <- numberOfConstraints AES.nistField (input Public <&> AES.sBox3)
      actual @?= Just expected

testAES128Count :: Int -> Assertion
testAES128Count expected = do
  actual <- numberOfConstraints AES.nistField AES.runCipher128
  actual @?= Just expected

numberOfConstraints :: (Encode a) => FieldType -> Comp a -> IO (Maybe Int)
numberOfConstraints field program = do
  result <- compile field program
  case result of
    Left _ -> return Nothing
    Right r1cs -> return $ Just (length (R1CS.toR1Cs r1cs))
