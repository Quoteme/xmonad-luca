{-# LANGUAGE TemplateHaskell #-}

module Layouts.Helpers.Tree where

import Control.Lens
import Control.Lens.TH
import Named

data Tree a b = Branch a [Tree a b] | Leaf b
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