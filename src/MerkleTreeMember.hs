{-# LANGUAGE DeriveFoldable #-}
module MerkleTreeMember where

import Data.Foldable (foldlM)
import Hash.Poseidon
import Keelung
import Text.Read (Lexeme(Number))
import qualified Lib.Array as Arr
import Data.Vector.Unboxed.Base (Vector(V_Down))
import GHC.Arr (indices)

mkTree :: [Number] -> Comp Number
mkTree xs = do
    nodes <- mkNodes xs
    case nodes of
        [x] -> return x
        xs  -> mkTree xs
  where mkNodes :: [Number] -> Comp [Number]
        mkNodes xs = do
            node <- hash $ toArray $ take 5 xs
            rest <- case drop 5 xs of
                [] -> return []
                xs -> mkNodes xs 
            return $ node : rest

-- Return the root as proof
getMerkleProof :: Number -> Arr (Arr Number) -> Arr Number -> Comp Number
getMerkleProof leaf siblings indices = do
  (_, digest) <-
    foldlM
      ( \(i, digest) p -> do
          assert (digest `Eq` choose p (access indices i))
          p' <- hash p >>= reuse
          return (i + 1, p')
      )
      (0, leaf)
      siblings
  return digest


-- Quinary merkle tree
checkMerkleProof :: Int -> Number -> Comp ()
checkMerkleProof depth root = do
  leaf <- inputNum
  siblings <- inputs2 depth 5
  indices <- inputs depth
  (_, digest) <-
    foldlM
      ( \(i, digest) p -> do
          assert (digest `Eq` choose p (access indices i))
          p' <- hash p >>= reuse
          return (i + 1, p')
      )
      (0, leaf)
      siblings
  assert (digest `Eq` root)

choose :: Arr Number -> Number -> Number
choose xs i =
  cond (i `Eq` 0) (access xs 0) $
    cond (i `Eq` 1) (access xs 1) $
      cond (i `Eq` 2) (access xs 2) $
        cond (i `Eq` 3) (access xs 3) $
          access xs 4

data Tree a = Node (Tree a) (Tree a) a | Leaf a

getRoot :: Tree a -> a
getRoot (Node _ _ n) = n
getRoot (Leaf n) = n

type MerkleTree = Tree Number
data TaggedPair a = Fst a a | Snd a a
type Path = Arr (TaggedPair Number)

dfs :: MerkleTree -> Comp (Maybe Path)
dfs tree = do
    leaf <- inputNum
    return $ dfs' tree leaf
  where
    dfs' :: MerkleTree -> Number -> Maybe Path
    dfs' (Node t1 t2 node) n =
        if node == n then
            Just $ toArray []
        else case (dfs' t1 n, dfs' t2 n) of
            (Nothing , Nothing) -> Nothing
            (Just p, _) -> Just $ Arr.cons (Fst (getRoot t1) (getRoot t2)) p
            (_, Just p) -> Just $ Arr.cons (Snd (getRoot t1) (getRoot t2)) p
    dfs' (Leaf l) n = if l == n then Just (toArray []) else Nothing