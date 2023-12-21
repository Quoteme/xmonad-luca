module LayoutModifiers.InterpolationModifier where
import XMonad
import XMonad.Layout.LayoutModifier
import Data.Time.Clock.POSIX
import Data.Maybe
import Control.Concurrent
import Control.Monad.Primitive (unsafePrimToIO)
import XMonad.Layout.Decoration

data InterpolationModifier a = InterpolationModifier
  { -- | The interpolation function
    --   The first argument is the passed time as a rational number between 0 and 1
    --   The second argument is the rectangle at the start
    --   The third argument is the rectangle at the end
    --   The result is the rectangle at the current time
    interpolationFunction :: Rational -> Rectangle -> Rectangle -> Rectangle
    -- | The time it takes to interpolate from the start to the end in seconds
    , interpolationTime :: Rational
    -- | The positions of all rectangles before they were moved.
    --   We need to store this information, because we only know the positions of the rectangles
    --   after they have been moved otherwise. The previous positions are needed to interpolate
    --   between the old and the new positions.
    --   Once the interpolation is finished, we update this field.
    , lastPositions :: [(a, Rectangle)]
    -- | The time at which we last updated the lastPositions field
    , lastUpdate :: Maybe POSIXTime
  }

instance Show a => Show (InterpolationModifier a) where
  show (InterpolationModifier _ t l _) = "InterpolationModifier " ++ show t ++ " " ++ show l

instance Read a => Read (InterpolationModifier a) where
  readsPrec _ s = [(InterpolationModifier defaultInterpolationFunction 0 [] Nothing, "")]

instance (Read a, Show a, Eq a) => LayoutModifier InterpolationModifier a where
  modifierDescription (InterpolationModifier _ t _ _) = "Interpolated " ++ show t
  redoLayout (InterpolationModifier f dt lastpos startTime) rect possibleStack wrs = do
    -- get the current time
    time <- io getPOSIXTime
    -- if no `lastUpdate` has been set, set it to the current time and we are done
    if isNothing startTime
      then return (wrs, Just $ InterpolationModifier f dt lastpos (Just time))
      else do
        -- Otherwise, we need to interpolate the positions of the rectangles
        -- 1. Get the percentage we are done with interpolating
        let timePassed = time - fromJust startTime
        let percentage = realToFrac timePassed / realToFrac dt
        if percentage >= 1
          -- If we are done, we can just return the new positions and reset the `lastPositions` field
          then return (wrs, Just $ InterpolationModifier f dt [] Nothing)
          else do
            -- 2. For any window which is not in our `lastPositions` list, we can 
            --    just keep its new position
            let newWindows = [(w, r) | (w, r) <- wrs, w `notElem` map fst lastpos]
            -- 3. For any window which is in our `lastPositions` list, we need to
            --    interpolate between the old and the new position
            let oldWindows = [(w, r) | (w, r) <- wrs, w `elem` map fst lastpos]
            let interpolatedWindows = [ (w, f percentage r1 r2) | (w, r1) <- oldWindows, (w2, r2) <- lastpos, w == w2 ]
            -- 4. Start a new thread that will sleep for 1/10th of a second and then call `redoLayout` again
            --    This is necessary, because we need to update the positions of the windows like an animation.
            liftIO . forkIO $ do
              threadDelay 100000
              -- liftIO $ redoLayout (InterpolationModifier f dt interpolatedWindows (Just time)) _ _ wrs
              -- unsafePrimToIO $ redoLayout (InterpolationModifier f dt interpolatedWindows (Just time)) rect possibleStack wrs
              return ()
            return (newWindows ++ interpolatedWindows, Just $ InterpolationModifier f dt lastpos (Just time))

defaultInterpolationFunction :: Rational -> Rectangle -> Rectangle -> Rectangle
defaultInterpolationFunction percentage (Rectangle x1 y1 w1 h1) (Rectangle x2 y2 w2 h2) =
      Rectangle (x1 + fi (round $ percentage * fi (x2 - x1))) (y1 + fi (round $ percentage * fi (y2 - y1))) (w1 + fi (round $ percentage * fi (w2 - w1))) (h1 + fi (round $ percentage * fi (h2 - h1)))

animate :: l a -> InterpolationModifier a
animate l = InterpolationModifier { interpolationFunction = defaultInterpolationFunction
                                  , interpolationTime = 0
                                  , lastPositions = []
                                  , lastUpdate = Nothing
                                  }