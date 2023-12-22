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

data InterpolationModifier a = InterpolationModifier
  { -- | The interpolation function
    --   The first argument is the passed time as a rational number between 0 and 1
    --   The second argument is the rectangle at the start
    --   The third argument is the rectangle at the end
    --   The result is the rectangle at the current time
    interpolationFunction :: Rational -> Rectangle -> Rectangle -> Rectangle
    -- | The initial positions of the windows
    , initialPositions :: [(a, Rectangle)]
    -- | The time it takes to interpolate from the start to the end in milliseconds
    , interpolationTime :: Rational
    -- | Number of frames to draw
    , fps :: Int
  }

instance Show a => Show (InterpolationModifier a) where
  show (InterpolationModifier _ _ dt fps) = "InterpolationModifier " ++ show dt ++ " " ++ show fps

instance Read a => Read (InterpolationModifier a) where
  readsPrec _ s = [(InterpolationModifier defaultInterpolationFunction [] 3000 10, "")]

instance LayoutModifier InterpolationModifier Window where
  modifierDescription _ = "Interpolated"

  redoLayout (InterpolationModifier f ip dt fps) rect possibleStack wrs = do
    -- get the current time
    time <- io getPOSIXTime
    -- calculate the number of frame we need to draw
    let frames = ceiling $ dt * fi fps / 1000
    -- calculate the time between two frames
    let frameTime = dt / fromIntegral frames
    -- calculate in how many milliseconds we need to draw which frame
    let percentages :: [Rational] = [fi i / fi frames | i <- [0..frames]]
    -- position the windows into their new rectangles (using X11 functions).
    -- Then wait i*frameTime milliseconds and repeat.
    -- logging (write to /tmp/xmonad.log) is done using `io`
    io $ appendFile "/tmp/xmonad.log" $ "\n\n---\n"
    io $ appendFile "/tmp/xmonad.log" $ "InterpolationModifier: " ++ show (length percentages) ++ " " ++ show (length wrs) ++ "\n"
    -- io $ appendFile "/tmp/xmonad.log" $ "🪟🔁" ++ show ip
    io $ do
      -- moveAll
      dpy <- openDisplay ""
      forM_ percentages $ \percentage -> do
        -- wait until the next frame
        threadDelay $ round (frameTime * percentage*500)
        -- for each window for which we saved its initial position and for which we have a new position
        -- interpolate between the two positions and move the window to the new position
        let interpolatings = [(a,r1,r2) | (a,r1) <- ip, Just r2 <- [lookup a wrs]]
        -- transform each `a` into a window id
        forM_ interpolatings $ \(a,r1,r2) -> do
          let r = f percentage r1 r2
          appendFile "/tmp/xmonad.log" $ format "🔁 {0}" [show a]
          -- first move the window to the upper left corner using `moveWindow`
          -- moveWindow dpy a (rect_x r) (rect_y r)
          moveWindow dpy a 0 0
          -- then resize the window using `resizeWindow`
          resizeWindow dpy a 10 10
          -- force the windows to be redrawn by sending an expose event
          allocaXEvent $ \ev -> do
            setEventType ev expose
            sendEvent dpy a False exposureMask ev
          flush dpy
      closeDisplay dpy
    return (wrs, Just $ InterpolationModifier f wrs dt fps)

defaultInterpolationFunction :: Rational -> Rectangle -> Rectangle -> Rectangle
defaultInterpolationFunction percentage (Rectangle x1 y1 w1 h1) (Rectangle x2 y2 w2 h2) =
      Rectangle (x1 + fi (round $ percentage * fi (x2 - x1))) (y1 + fi (round $ percentage * fi (y2 - y1))) (w1 + fi (round $ percentage * fi (w2 - w1))) (h1 + fi (round $ percentage * fi (h2 - h1)))


defaultInterpolationModifier :: InterpolationModifier a
defaultInterpolationModifier = InterpolationModifier {
  interpolationFunction = defaultInterpolationFunction
  , interpolationTime = 3000
  , initialPositions = []
  , fps = 10
  } 

animate :: l a -> ModifiedLayout InterpolationModifier l a
animate = ModifiedLayout defaultInterpolationModifier

moveAll :: IO ()
moveAll = do
  -- Open a connection to the X server.
  display <- openDisplay ""

  -- Get the root window.
  let root = defaultRootWindow display

  -- Get the list of all windows.
  (_, _, windows) <- queryTree display root

  -- For each window, move it to (0,0) and resize it to 10x10.
  forM_ windows $ \window -> do
    -- Move the window to the upper-left corner.
    moveWindow display window 0 0
    
    -- Resize the window to 10x10.
    resizeWindow display window 10 10

    -- Make the changes take effect immediately.
    sync display False

  -- Close the connection to the X server.
  closeDisplay display
