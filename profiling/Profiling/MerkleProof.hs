module Profiling.MerkleProof where

import Keelung
import Keelung.Prof
import Keelung.Error
import Keelung.Constraint.R1CS

import MerkleTree

prof :: IO (Either Error (R1CS Integer))
prof = compileProfWithOpts 1 [] (rtsoptMemory 16 8 256) GF181 (getMerkleProof' 10)
