module Semaphore where

import Hash.Poseidon
import MerkleTree
import Keelung
import Prelude

semaphore :: Field -> Field -> Arr (Arr Field) -> Arr Field
          -> Field -> Field
          -- Public: root of merkle tree, hash of nullifiers
          -> Comp (Field, Field)
semaphore identityNullifier identityTrapdoor siblings indices signalHash externalNullifier = do
    secret        <- hash $ toArray [identityNullifier, identityTrapdoor]
    commitment    <- hash $ toArray [secret]
    nullifierHash <- hash $ toArray [externalNullifier, identityNullifier]
    root          <- getMerkleProof commitment siblings indices
    signalHashSquared <- reuse $ signalHash * signalHash
    return (root, nullifierHash)

semaphore' :: Int
           -> Field
           -> Field
           -> Comp (Arr Field)
semaphore' depth signalHash externalNullifier = do
    identityNullifier <- inputField
    identityTrapdoor  <- inputField
    siblings <- inputs2 depth 5
    indices <- inputs depth
    (x,y) <- semaphore identityNullifier identityTrapdoor siblings indices signalHash externalNullifier
    return $ toArray [x,y]

-- Check two things: the signal is valid (cast by someone in a group), and it is not double signaling
checkSignal :: Int -> Field -> Arr Field -> Field -> Field -> Comp ()
checkSignal depth root nullifierMap signalHash externalNullifier = do
    identityNullifier <- inputField
    identityTrapdoor  <- inputField
    siblings <- inputs2 depth 5
    indices <- inputs depth

    -- check the commitment is in the tree
    (root', nullifierHash) <- semaphore identityNullifier identityTrapdoor siblings indices signalHash externalNullifier

    assert (root `eq` root')

    -- check the signal has not been cast
    assert (neg $ has nullifierMap nullifierHash)