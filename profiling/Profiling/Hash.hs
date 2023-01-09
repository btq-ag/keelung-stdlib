module Profiling.Hash where

import Keelung
import Keelung.Error
import Keelung.Constraint.R1CS
import Keelung.Prof
import MerkleTree

prof :: IO (Either Error (R1CS Integer))
prof = compileProfWithOpts 1 [] (rtsoptMemory 16 8 256 <> [ "-pohash" ]) GF181 (getMerkleProof' 10)
