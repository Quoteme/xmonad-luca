{-# LANGUAGE TemplateHaskell #-}

module Layouts.Helpers.Tree where

import Control.Lens
import Control.Lens.TH

data Tree a b = Branch a [Tree a b] | Leaf b

$(makePrisms ''Tree)
$(makeLenses ''Tree)

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