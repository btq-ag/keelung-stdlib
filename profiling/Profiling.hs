module Profiling where

import System.Directory

import Keelung.Error
import Keelung.Constraint.R1CS

import qualified Profiling.Hash as Hash
import qualified Profiling.MerkleProof as MerkleProof

main :: IO ()
main = runProfs [ (Hash.prof, "hash.prof")
                , (MerkleProof.prof, "MerkleProof.prof")
                ]

runProfs :: [(IO (Either Error (R1CS Integer)), String)] -> IO ()
runProfs [] = return ()
runProfs ((f, filename) : fs) = do
    r <- f
    case r of
        Left err -> fail ("Generating " <> filename <> ": " <> show err)
        Right _  -> renameFile "keelungc.prof" filename >> runProfs fs