module Layouts.Helpers.Involution where

class Involutive a where
  -- | An involution is a function that is its own inverse.
  -- | For example, the function 'not' is an involution.
  -- |
  -- | f (f x) = x
  involution :: a -> a