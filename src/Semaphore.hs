module Semaphore where

import Hash.Poseidon
import MerkleTree
import Keelung
import Prelude

semaphore :: Number -> Number -> Arr (Arr Number) -> Arr Number
          -> Number -> Number
          -- Public: root of merkle tree, hash of nullifiers
          -> Comp (Number, Number)
semaphore identityNullifier identityTrapdoor siblings indices signalHash externalNullifier = do
    secret        <- hash $ toArray [identityNullifier, identityTrapdoor]
    commitment    <- hash $ toArray [secret]
    nullifierHash <- hash $ toArray [externalNullifier, identityNullifier]
    root          <- getMerkleProof commitment siblings indices
    signalHashSquared <- reuse $ signalHash * signalHash
    return (root, nullifierHash)

-- Check two things: the signal is valid (cast by someone in a group), and it is not double signaling
checkSignal :: Int -> Number -> Arr Number -> Number -> Number -> Comp ()
checkSignal depth root nullifierMap signalHash externalNullifier = do
    identityNullifier <- inputNum
    identityTrapdoor  <- inputNum
    siblings <- inputs2 depth 5
    indices <- inputs depth

    -- check the commitment is in the tree
    (root', nullifierHash) <- semaphore identityNullifier identityTrapdoor siblings indices signalHash externalNullifier

    assert (root `Eq` root')

    -- check the signal has not been cast
    assert (neg $ has nullifierMap nullifierHash)