module LayoutModifiers.InterpolationModifier where
import XMonad
import XMonad.Layout.LayoutModifier
import Data.Time.Clock.POSIX
import Data.Maybe
import Control.Concurrent
import Control.Monad.Primitive (unsafePrimToIO)
import XMonad.Layout.Decoration
import Control.Monad
import XMonad.StackSet (currentTag)
import Text.Format
import XMonad.Util.PositionStore (getPosStore, posStoreQuery, posStoreInsert, modifyPosStore, posStoreRemove, posStoreMove)

-- | A layout modifier that adds an animation effect to moving and resizing windows
-- of windows.
--
-- We add this animation effect, by storing all positions which we receive 
-- from the `runLayout` function of the underlying layout, if we do not have any
-- position stored for a window.
--
-- If we do however have the positions of a window stored and we get a new position
-- from the underlying layout, we interpolate between the two positions.
-- We this this so long, until we reach the position we got from the underlying layout.
data InterpolationModifier a = InterpolationModifier
  { -- | The interpolation function
    --   The first argument is the rectangle where it currently is
    --   The second argument is the rectangle where it should be
    --   The result is the rectangle after one interpolation step
    interpolationFunction :: Rectangle -> Rectangle -> Rectangle
    -- amount of time an interpolation step takes in milliseconds
    , interpolationTime :: Int
  }

instance Show a => Show (InterpolationModifier a) where
  show (InterpolationModifier f dt) = "InterpolationModifier " ++ show dt

instance Read a => Read (InterpolationModifier a) where
  readsPrec _ s = [(InterpolationModifier (defaultInterpolationFunction 1 1 1 1) 3000, "")]

instance LayoutModifier InterpolationModifier Window where
  modifierDescription _ = "Interpolated"

  redoLayout (InterpolationModifier f dt) rect possibleStack wrs = do
    posStore <- getPosStore
    -- get all stored positions for any windows in `wrs`
    let storedPositions' = [(a', pos) | (a', pos) <- [(a, posStoreQuery posStore a rect) | a <- map fst wrs], isJust pos]
    io $ appendFile "/tmp/interpolation.log" $ format "1: {0}\n" [ show storedPositions' ]
    -- add all positions for windows in `wrs` which we do not have stored
    forM_ [(a, pos) | (a, pos) <- wrs, not $ any (\(a', _) -> a == a') storedPositions'] $ \(a,pos) -> do
      modifyPosStore (\ps -> posStoreInsert ps a pos rect)
    -- remove all positions for windows which are not in `wrs` from the store
    io $ appendFile "/tmp/interpolation.log" $ "2\n"
    forM_ [a | (a, _) <- storedPositions', not $ any (\(a', _) -> a == a') wrs] $ \a -> do
      modifyPosStore (`posStoreRemove` a)
    -- now that the store is synced, modify it by interpolating between the stored positions and the new positions
    io $ appendFile "/tmp/interpolation.log" $ "3\n"
    forM_ wrs $ \(a, wantedRect) -> do
      let currentRect = fromJust $ posStoreQuery posStore a rect
      modifyPosStore (\ps -> posStoreInsert ps a (f currentRect wantedRect) rect)
    io $ appendFile "/tmp/interpolation.log" $ "4\n"
    -- now, fetch all positions and windows from the posStore into `storedPositions`
    let storedPositions = [(a, fromJust $ posStoreQuery posStore a rect) | a <- map fst wrs]
    io $ appendFile "/tmp/interpolation.log" $ format "InterpolationModifier: {0}\n" [ show storedPositions ]
    return (storedPositions, Just $ InterpolationModifier f dt)

-- | Transform one rectangle into another
--
-- The first argument is how many pixels we should move towards the second rectangle in one step
-- The second argument is how many pixels we should resize towards the second rectangle in one step
-- The third argument is a threshold for distance. If we are this close to the second rectangle, we will just jump to it  
-- The fourth argument is a threshold for size. If we are this close to the second rectangle, we will just jump to it
-- the fifth argument is the rectangle we want to transform
-- The sixth argument is the rectangle we want to transform to
defaultInterpolationFunction :: Int -> Int -> Int -> Int -> Rectangle -> Rectangle -> Rectangle
defaultInterpolationFunction dt ds thrT thrS (Rectangle x y w h) (Rectangle x' y' w' h') =
  Rectangle (x + dx') (y + dy') (w + dw') (h + dh')
  where
    dx' = if abs (x - x') < fromIntegral thrT then x' - x else fromIntegral dt
    dy' = if abs (y - y') < fromIntegral thrT then y' - y else fromIntegral dt
    dw' = if abs (w - w') < fromIntegral thrS then w' - w else fromIntegral dt
    dh' = if abs (h - h') < fromIntegral thrS then h' - h else fromIntegral dt


defaultInterpolationModifier :: InterpolationModifier a
defaultInterpolationModifier = InterpolationModifier {
  interpolationFunction = defaultInterpolationFunction 1 1 1 1
  , interpolationTime = 3000
  }

animate :: l a -> ModifiedLayout InterpolationModifier l a
animate = ModifiedLayout defaultInterpolationModifier