module MerkleTree where

import Data.Foldable (foldlM)
import Hash.Poseidon
import Keelung

mkTree :: [Field] -> Comp Field
mkTree xs = do
    nodes <- mkNodes xs
    case nodes of
        [x] -> return x
        xs  -> mkTree xs
  where mkNodes :: [Field] -> Comp [Field]
        mkNodes xs = do
            node <- hash $ take 5 xs
            rest <- case drop 5 xs of
                [] -> return []
                xs -> mkNodes xs 
            return $ node : rest

-- Return the root as proof
getMerkleProof :: Field -> [[Field]] -> [Field] -> Comp Field
getMerkleProof leaf siblings indices = do
    foldlM
      ( \digest (i, p) -> do
          assert (digest `eq` choose p i)
          hash p >>= reuse
      )
      leaf
      (zip indices siblings)

getMerkleProof' :: Int -> Comp Field
getMerkleProof' depth = do
  leaf <- inputField
  siblings <- inputList2 depth 5
  indices <- inputList depth
  getMerkleProof leaf siblings indices

choose :: [Field] -> Field -> Field
choose []       _ = 0
choose (x : xs) i = cond (i `eq` (4 - Integer (fromIntegral $ length xs))) x $ choose xs i

data Tree a = Node (Tree a) (Tree a) a | Leaf a


getRoot :: Tree a -> a
getRoot (Node _ _ n) = n
getRoot (Leaf n) = n

type MerkleTree = Tree Field
data TaggedPair a = Fst a a | Snd a a
type Path = [TaggedPair Field]

dfs :: MerkleTree -> Comp (Maybe Path)
dfs tree = do
    leaf <- inputField
    return $ dfs' tree leaf
  where
    dfs' :: MerkleTree -> Field -> Maybe Path
    dfs' (Node t1 t2 node) n =
        if node == n then
            Just []
        else case (dfs' t1 n, dfs' t2 n) of
            (Nothing , Nothing) -> Nothing
            (Just p, _) -> Just (Fst (getRoot t1) (getRoot t2) : p)
            (_, Just p) -> Just (Snd (getRoot t1) (getRoot t2) : p)
    dfs' (Leaf l) n = if l == n then Just [] else Nothing
