module Profiling.Hash where

import Keelung
import Keelung.Error
import Keelung.Constraint.R1CS
import Keelung.Prof
import MerkleTree

prof :: String -> IO (Either Error (R1CS Integer))
prof path = compileProfWithOpts 1 [] (rtsoptMemory 6 6 256 <> [ "-po" <> path <> "/hash" ]) GF181 (getMerkleProof' 10)
