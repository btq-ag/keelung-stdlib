module Profiling.MerkleProof where

import Keelung
import Keelung.Prof
import Keelung.Error
import Keelung.Constraint.R1CS

import MerkleTree

prof :: String -> IO (Either Error (R1CS Integer))
prof path = compileProfWithOpts 1 [] (rtsoptMemory 6 6 256 <> [ "-po" <> path <> "/MerkleProof" ]) GF181 (getMerkleProof' 10)
