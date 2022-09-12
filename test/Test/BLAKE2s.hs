{-# LANGUAGE DataKinds #-}

module Test.BLAKE2s where

import BLAKE2s
import Data.Word
import Keelung
import qualified Lib.Array as Array
import qualified Lib.W32 as W32
import qualified Lib.W8 as W8
import Test.Tasty
import Test.Tasty.HUnit
import Test.Util (toBitArr, toWord32List)

tests :: TestTree
tests =
  testGroup
    "Blake2s"
    [ testCase "init state" $
        testIh 0 32,
      testCase "round 0" $
        testCompress'
          0
          "abc"
          [ 0x6B08E647,
            0xBB67AE85,
            0x3C6EF372,
            0xA54FF53A,
            0x510E527F,
            0x9B05688C,
            0x1F83D9AB,
            0x5BE0CD19,
            0x6A09E667,
            0xBB67AE85,
            0x3C6EF372,
            0xA54FF53A,
            0x510E527C,
            0x9B05688C,
            0xE07C2654,
            0x5BE0CD19
          ],
      testCase "round 1" $
        testCompress'
          1
          "abc"
          [ 0x16A3242E,
            0xd7b5e238,
            0xce8ce24b,
            0x927aede1,
            0xa7b430d9,
            0x93a4a14e,
            0xa44e7c31,
            0x41d4759b,
            0x95bf33d3,
            0x9a99c181,
            0x608a3a6b,
            0xb666383e,
            0x7a8dd50f,
            0xbe378ed7,
            0x353d1ee6,
            0x3bb44c6b
          ],
      testCase "round 2" $
        testCompress'
          2
          "abc"
          [ 0x3AE30FE3,
            0x0982A96B,
            0xE88185B4,
            0x3E339B16,
            0xF24338CD,
            0x0E66D326,
            0xE005ED0C,
            0xD591A277,
            0x180B1F3A,
            0xFCF43914,
            0x30DB62D6,
            0x4847831C,
            0x7F00C58E,
            0xFB847886,
            0xC544E836,
            0x524AB0E2
          ],
      testCase "round 3" $
        testCompress'
          3
          "abc"
          [ 0x7A3BE783,
            0x997546C1,
            0xD45246DF,
            0xEDB5F821,
            0x7F98A742,
            0x10E864E2,
            0xD4AB70D0,
            0xC63CB1AB,
            0x6038DA9E,
            0x414594B0,
            0xF2C218B5,
            0x8DA0DCB7,
            0xD7CD7AF5,
            0xAB4909DF,
            0x85031A52,
            0xC4EDFC98
          ],
      testCase "round 4" $
        testCompress'
          4
          "abc"
          [ 0x2A8B8CB7,
            0x1ACA82B2,
            0x14045D7F,
            0xCC7258ED,
            0x383CF67C,
            0xE090E7F9,
            0x3025D276,
            0x57D04DE4,
            0x994BACF0,
            0xF0982759,
            0xF17EE300,
            0xD48FC2D5,
            0xDC854C10,
            0x523898A9,
            0xC03A0F89,
            0x47D6CD88
          ],
      testCase "round 5" $
        testCompress'
          5
          "abc"
          [ 0xC4AA2DDB,
            0x111343A3,
            0xD54A700A,
            0x574A00A9,
            0x857D5A48,
            0xB1E11989,
            0x6F5C52DF,
            0xDD2C53A3,
            0x678E5F8E,
            0x9718D4E9,
            0x622CB684,
            0x92976076,
            0x0E41A517,
            0x359DC2BE,
            0x87A87DDD,
            0x643F9CEC
          ],
      testCase "round 6" $
        testCompress'
          6
          "abc"
          [ 0x3453921C,
            0xD7595EE1,
            0x592E776D,
            0x3ED6A974,
            0x4D997CB3,
            0xDE9212C3,
            0x35ADF5C9,
            0x9916FD65,
            0x96562E89,
            0x4EAD0792,
            0xEBFC2712,
            0x2385F5B2,
            0xF34600FB,
            0xD7BC20FB,
            0xEB452A7B,
            0xECE1AA40
          ],
      testCase "round 7" $
        testCompress'
          7
          "abc"
          [ 0xBE851B2D,
            0xA85F6358,
            0x81E6FC3B,
            0x0BB28000,
            0xFA55A33A,
            0x87BE1FAD,
            0x4119370F,
            0x1E2261AA,
            0xA1318FD3,
            0xF4329816,
            0x071783C2,
            0x6E536A8D,
            0x9A81A601,
            0xE7EC80F1,
            0xACC09948,
            0xF849A584
          ],
      testCase "round 8" $
        testCompress'
          8
          "abc"
          [ 0x07E5B85A,
            0x069CC164,
            0xF9DE3141,
            0xA56F4680,
            0x9E440AD2,
            0x9AB659EA,
            0x3C84B971,
            0x21DBD9CF,
            0x46699F8C,
            0x765257EC,
            0xAF1D998C,
            0x75E4C3B6,
            0x523878DC,
            0x30715015,
            0x397FEE81,
            0x4F1FA799
          ],
      testCase "round 9" $
        testCompress'
          9
          "abc"
          [ 0x435148C4,
            0xA5AA2D11,
            0x4B354173,
            0xD543BC9E,
            0xBDA2591C,
            0xBF1D2569,
            0x4FCB3120,
            0x707ADA48,
            0x565B3FDE,
            0x32C9C916,
            0xEAF4A1AB,
            0xB1018F28,
            0x8078D978,
            0x68ADE4B5,
            0x9778FDA3,
            0x2863B92E
          ],
      testCase "round 10" $
        testCompress'
          10
          "abc"
          [ 0xD9C994AA,
            0xCFEC3AA6,
            0x700D0AB2,
            0x2C38670E,
            0xAF6A1F66,
            0x1D023EF3,
            0x1D9EC27D,
            0x945357A5,
            0x3E9FFEBD,
            0x969FE811,
            0xEF485E21,
            0xA632797A,
            0xDEEF082E,
            0xAF3D80E1,
            0x4E86829B,
            0x4DEAFD3A
          ]
    ]

testIh :: Word32 -> Word32 -> Assertion
testIh kk nn = do
  actual <- interpret_ GF181 (return $ ih kk nn) ([] :: [GF181])
  fmap toWord32List actual
    @?= Right
      [ 0x6b08e647,
        0xbb67ae85,
        0x3c6ef372,
        0xa54ff53a,
        0x510e527f,
        0x9b05688c,
        0x1f83d9ab,
        0x5be0cd19
      ]

testCompress' :: Int -> String -> [Word32] -> Assertion
testCompress' r msg expect = do
  let actual = compress' r (W32.fromW8Chunks' . W8.pad' 64 . W8.fromString' $ msg) (fromIntegral $ length msg) True (ih 0 32)
  actual' <- interpret_ GF181 (return actual) ([] :: [GF181])
  fmap toWord32List actual' @?= Right expect
