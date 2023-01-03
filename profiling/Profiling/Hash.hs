module Profiling.Hash where

import Keelung
import Keelung.Prof
import Keelung.Field
import MerkleTree

main :: IO ()
main = compileProfWithOpts 1 (optMemory 16 8 256) GF181 (getMerkleProof' 10) >> return ()
