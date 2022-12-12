module Semaphore.Semaphore where

import Hash.Poseidon
import MerkleTreeMember
import Keelung
import Prelude

createGroup = mkTree


semaphore :: Number -> Number -> Arr (Arr Number) -> Arr Number
          -> Number -> Number
          -- Outpus a root of merkle tree and a hash of nullifiers
          -> Comp (Number, Number)
semaphore identityNullifier identityTrapdoor siblings indices signalHash externalNullifier = do
    secret <- hash $ toArray [identityNullifier, identityTrapdoor]
    nullifierHash <- hash $ toArray [externalNullifier, identityNullifier]
    commitment <- hash $ toArray [secret]
    root <- getMerkleProof commitment siblings indices
    return (root, nullifierHash)

-- Check two things: the signal is valid (cast by someone in a group), and it is not double signaling
checkSignalHash :: Int -> Number -> Arr Number -> Number -> Number -> Comp ()
checkSignalHash depth root nullifierMap signalHash externalNullifier = do
    identityNullifier <- inputNum
    identityTrapdoor <- inputNum
    secret <- hash $ toArray [identityNullifier, identityTrapdoor]
    nullifierHash <- hash $ toArray [externalNullifier, identityNullifier]

    -- check commitment is in the tree
    commitment <- hash $ toArray [secret]
    path <- inputs2 depth 5
    indices <- inputs depth
    checkMerkleProof root commitment path indices

    -- check the signal is calculated from the identity

    -- check the signal has not been cast

    assert (neg $ has nullifierMap nullifierHash)
    