module Profiling where

import System.Directory
import System.Environment

import Keelung.Error
import Keelung.Constraint.R1CS

import qualified Profiling.Hash as Hash
import qualified Profiling.MerkleProof as MerkleProof

main :: IO ()
main = do args <- getArgs
          let path = args !! 0
          runProfs [ (Hash.prof path)
                   , (MerkleProof.prof path)
                   ]

runProfs :: [IO (Either Error (R1CS Integer))] -> IO ()
runProfs [] = return ()
runProfs (f : fs) = do
    r <- f
    case r of
        Left err -> fail ("Generating prof: " <> show err)
        Right _  -> runProfs fs
