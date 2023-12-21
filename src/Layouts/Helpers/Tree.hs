{-# LANGUAGE TemplateHaskell #-}

module Layouts.Helpers.Tree where

import Control.Lens
import Control.Lens.TH
import Control.Monad
import Data.Maybe
import Layouts.Helpers.Involution
import Named

data Tree a b = Branch {trunk :: a, branches :: [Tree a b]} | Leaf b
  deriving (Read, Eq)

$(makePrisms ''Tree)
$(makeLenses ''Tree)

instance (Show a, Show b) => Show (Tree a b) where
  show (Leaf b) = "🌿" ++ show b
  -- add indentation to make the tree structure more clear
  show (Branch a bs) = "🌳" ++ show a ++ "\n" ++ unlines (map ("  " ++) (lines $ show bs))

instance Bifunctor Tree where
  bimap :: (a -> c) -> (b -> d) -> Tree a b -> Tree c d
  bimap _ g (Leaf b) = Leaf (g b)
  bimap f g (Branch a bs) = Branch (f a) (map (bimap f g) bs)

instance Functor (Tree a) where
  fmap :: (b -> c) -> Tree a b -> Tree a c
  fmap = bimap id

instance Foldable (Tree a) where
  foldr :: (b -> c -> c) -> c -> Tree a b -> c
  foldr f z (Leaf b) = f b z
  foldr f z (Branch _ bs) = foldr (flip (foldr f)) z bs

-- | A path represents a location in a tree from the root to possibly
-- | a leaf or branch
type Path = [Int]

-- | Check if a path actually exists in a tree
pathExists :: Path -> Tree a b -> Bool
pathExists [] _ = True
pathExists (i : is) (Branch _ bs) = i < length bs && pathExists is (bs !! i)

-- | Given an element which might be in a tree, find the path to that element.
-- |
-- | We use a depth-first search to find the path to the element.
-- |
-- | Example:
-- | find 1 (Branch "a" []) = Nothing
-- |
-- | Example:
-- | find 1 (Branch "a" [Branch "a" []]) = Nothing
-- |
-- | Example:
-- | find 1 (Branch "a" [Branch "a" [], (Leaf 1)]) = Just [1]
-- |
-- | Example:
-- | find 1 (Branch "a" [Branch "a" [(Leaf 1)], (Leaf 1)]) = Just [0, 0]
-- |
-- | Example:
-- | find 1 (Leaf 1) = Just []
-- |
-- | Example:
-- | find 3 (Branch 1 [Branch 2 [Leaf 3, Leaf 4], Leaf 5]) = Just [0, 0]
-- |
-- | Example (complex):
-- | find 7 (
-- | Branch "a" [Branch "b" [Leaf 1, Leaf 2], Branch "c" [Leaf 3, Branch "d" [Branch "e" [Leaf 4, Leaf 5], Branch "f" [Leaf 6, Leaf 7]]]]
-- | ) = Just [1, 1, 1, 1]
find :: Eq b => b -> Tree a b -> Maybe Path
find b t = find' b t []

-- | Helper function for [find]
find' :: Eq b => b -> Tree a b -> Path -> Maybe Path
find' b (Leaf x) path
  | b == x = Just path
  | otherwise = Nothing
find' b (Branch _ bs) path = firstNonNothing
  where
    foundInSublist = [find' b sub (path ++ [i]) | (i, sub) <- zip [0 ..] bs]
    firstNonNothing = listToMaybe $ catMaybes foundInSublist

-- | Get the subtree at the given path
cut :: Path -> Tree a b -> Maybe (Tree a b)
cut [] t = Just t
cut (i : is) (Branch _ bs) = cut is =<< bs ^? ix i

-- | Append a new tree to an old tree at a given path
append ::
  -- function that takes the current branch data at the path
  -- and returns the new branch data
  (Maybe a -> a) ->
  -- path to append to
  Path ->
  -- original tree
  Tree a b ->
  -- new tree
  Tree a b ->
  -- the new tree if the path exists
  Maybe (Tree a b)
append f [] (Leaf b) t = Just $ Branch (f Nothing) [(Leaf b), t]
append f [] (Branch a bs) t = Just $ Branch (f $ Just a) (bs ++ [t])
append f (i : is) (Branch a bs) t = do
  b <- bs ^? ix i
  t' <- append f is b t
  return $ Branch a (bs & ix i .~ t')
append _ _ _ _ = Nothing

-- | Rotate a branch at a given path
-- |
rotate :: Path -> Tree a b -> Maybe (Tree a b)
rotate [] (Leaf b) = Just $ Leaf b
rotate [] (Branch a bs) = Just $ Branch a (tail bs ++ [head bs])
rotate (i : is) (Branch a bs) = do
  b <- bs ^? ix i
  b' <- rotate is b
  return $ Branch a (bs & ix i .~ b')
rotate _ _ = Nothing

-- | Remove all leafs matching a predicate
removeLeafs :: (b -> Bool) -> Tree a b -> Maybe (Tree a b)
removeLeafs p (Leaf b) = if p b then Nothing else Just $ Leaf b
removeLeafs p (Branch a bs) = Just $ Branch a [b | Just b <- map (removeLeafs p) bs]

-- | Add new leafs for a sequence of values at a given path
addAsLeafs :: (Maybe a -> a) -> Path -> Tree a b -> [b] -> Maybe (Tree a b)
addAsLeafs f p t [] = Just t
addAsLeafs f p t (b : bs) = do
  t' <- append f p t (Leaf b)
  addAsLeafs f p t' bs

-- | Given a sequence of values, we check which ones are currently not
-- | in the tree and add the missing ones as leafs
addMissingAsLeafs :: Eq b => (Maybe a -> a) -> Path -> Tree a b -> [b] -> Maybe (Tree a b)
addMissingAsLeafs f p t bs = do
  -- get all the values that are not in the tree
  let missing = filter (`notElem` t) bs
  -- add the missing values as leafs
  addAsLeafs f p t missing

-- | Update a tree with a list of values it should contain in its leafs.
-- | We remove all leafs that are not in the list of values and add all
-- | missing values as leafs at the given path.
updateLeafs :: Eq b => (Maybe a -> a) -> Path -> Tree a b -> [b] -> Maybe (Tree a b)
updateLeafs f p t bs = do
  -- remove all leafs that are not in the list of values
  t' <- removeLeafs (`notElem` bs) t
  -- add all values that are not in the tree as leafs
  addMissingAsLeafs f p t' bs

-- | Apply a function to a subtree at a given path
apply :: Path -> (Tree a b -> Tree a b) -> Tree a b -> Maybe (Tree a b)
apply [] f t = Just $ f t
apply (i : is) f (Branch a bs) = do
  b <- bs ^? ix i
  b' <- apply is f b
  return $ Branch a (bs & ix i .~ b')
apply _ _ _ = Nothing

-- | Reverse a tree.
-- |
-- | Example
-- |
-- |    A
-- |   / \
-- |  B   C
-- |     / \
-- |    D   E
-- |
-- | becomes
-- |
-- |    A
-- |   / \
-- |  C   B
-- | / \
-- | E   D
instance Involutive (Tree a b) where
  involution :: Tree a b -> Tree a b
  involution (Leaf b) = Leaf b
  involution (Branch a bs) = Branch a (reverse $ map involution bs)

-- | Clean a tree by removing all branches that have no leafs
clean :: Tree a b -> Tree a b
clean (Leaf b) = Leaf b
clean (Branch a [x]) = clean x
clean (Branch a bs) = Branch a (filter (not . emptyBranch) $ map clean bs)
  where
    emptyBranch (Leaf _) = False
    emptyBranch (Branch _ bs) = all emptyBranch bs

-- | Like [clean], but replace maximal empty branches with a single empty branch
shallowClean :: Tree a b -> Tree a b
shallowClean (Leaf b) = Leaf b
shallowClean (Branch a bs) 
  | all emptyBranch cleanedBranches = Branch a []
  | otherwise = Branch a (filter (not . emptyBranch) cleanedBranches)
  where
    emptyBranch (Leaf _) = False
    emptyBranch (Branch _ bs) = all emptyBranch bs
    cleanedBranches = map shallowClean bs

-- | Get the number ot subtrees in a branch at a given path
numSubtrees :: Path -> Tree a b -> Maybe Int
numSubtrees p t = do
  t' <- cut p t
  return $ case t' of
    Leaf _ -> 0
    Branch _ bs -> length bs

-- | Given a path and a tree, find the longest initial subpath, such that
-- the subtree spanned by that path is a branch
--
-- Example:
--
-- branchSubpath [0, 0, 0] (Branch "a" [Branch "b" [Leaf 1, Leaf 2], Branch "c" [Leaf 3, Leaf 4]]) = [0]
branchSubpath :: Path -> Tree a b -> Path
branchSubpath p (Leaf _) = []
branchSubpath (i : is) (Branch _ bs) =
  if (i < length bs) && isBranch (bs !! i) then i : branchSubpath is (bs !! i) else []
  where
    isBranch (Branch _ _) = True
    isBranch _ = False