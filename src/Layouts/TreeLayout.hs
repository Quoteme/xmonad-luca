module Layouts.TreeLayout
  ( TreeLayout (..),
    emptyTreeLayout,
  )
where

import Control.Monad
import Data.Bifunctor
import Data.Bits
import Data.Colour.RGBSpace
import Data.Colour.RGBSpace.HSV
import Data.List (inits)
import Data.Maybe
import Layouts.Helpers.Direction
import Layouts.Helpers.Involution
import Layouts.Helpers.Tree
import Safe
import Text.Format
import XMonad
import XMonad.Layout.BinarySpacePartition (Rotate (Rotate), SelectMoveNode (..), Swap (..))
import XMonad.StackSet

-- | Container to store all the extra-data for a node in our [TreeLayout].
data WindowNode = WindowNode
  { percentage :: Rational,
    window :: Window
  }
  deriving (Show, Read)

-- | We define two [WindowNode]s to be equal if their [window]s are equal.
-- |
-- | In practice, this means we would consider two [WindowNode Window]s to be
-- | equal if they store the same window.
instance Eq WindowNode where
  (==) :: WindowNode -> WindowNode -> Bool
  (==) (WindowNode _ v1) (WindowNode _ v2) = v1 == v2

data BranchNode = BranchNode
  { rotation :: Direction
  }
  deriving (Show, Read, Eq)

-- | A datatype to represent a tree layout of windows for XMonad.
data TreeLayout a = TreeLayout
  { tree :: Tree BranchNode WindowNode,
    defaultBranch :: BranchNode,
    currentPath :: Path,
    lastFocused :: Maybe Window
  }
  deriving (Show, Read, Eq)

instance LayoutClass TreeLayout Window where
  -- \| The name we give to our layout.
  description _ = "TreeLayout"

  -- \| This function will place the windows on the screen (some given [rect]).
  doLayout (TreeLayout tree defaultBranch currentPath lastFocused) rect stack = do
    -- get the current desktop dimensions
    let Rectangle sx sy sw sh = rect
    -- get the currently focused window
    focused <- gets (peek . windowset) :: X (Maybe Window)
    -- get all the windows in the stack
    let windows = XMonad.StackSet.integrate stack
    -- wrap each window in a [WindowNode]
    let windowNodes = map (WindowNode 1) windows
    let tree' = do
          -- -- remove all windows that are not in the stack
          -- oldWindowsRemoved <- removeLeafs (`notElem` windowNodes) tree
          -- let newWindows = [w | w <- windowNodes, w `notElem` tree]
          -- -- make room for the new windows
          -- current <- percentages currentPath oldWindowsRemoved
          -- let want = current / fromIntegral (length newWindows)
          -- -- calculate the factor by which we need to multiply the percentages
          -- -- of each window in the current path to make them add up to 1-want
          -- let factor = (current - want) / current
          -- freedTree <- applyPercentages factor currentPath oldWindowsRemoved
          -- let newWindows' = map (\w -> w {percentage = want}) newWindows
          -- -- now we only need to add the new windows to the tree
          -- addAsLeafs (const defaultBranch) currentPath freedTree newWindows'
          updateLeafs (fromMaybe defaultBranch) (initDef [] currentPath) tree windowNodes
    -- add all new windows to the tree
    -- let tree' = updateLeafs (fromMaybe defaultBranch) (initDef [] currentPath) tree windowNodes
    -- define the new tree.
    -- If the focused window has changed, we want to clean the tree.
    let tree'' =
          fromMaybe tree $
            if focused /= lastFocused
              then clean <$> tree'
              else shallowClean <$> tree'
    -- if the focused window has changed from the last time, we want to recalculate the currentPath
    -- \| To be able to search for our focused window, we first need to convert our
    -- \| [Tree BranchNode WindowNode] to a [Tree BranchNode Window].
    -- \| This is, because we search a [Window] and not a [WindowNode].
    -- \| We can do that by using the [bimap] (or here the [second = bimap id]) function.
    let windowTree = second window tree''
    let newpath = if isJust focused then fromMaybe [] $ find (fromJust focused) windowTree else []
    -- let newpath' = if focused /= lastFocused then currentPath else newpath
    let newpath' = newpath
    -- calculate the new positions of the windows
    let rects = layoutRects tree'' rect
    -- draw a colorful rectangle around the focused window
    -- the saturation of the border is dependent on the depth of the focused window
    -- the deeper the window, the more saturated the border
    --
    -- 1. We get all the partial paths of the current path
    --    Example: [1,2,3] -> [[1],[1,2],[1,2,3]]
    -- let partialPaths = (tail . inits) newpath'
    let partialPaths = inits newpath'
    forM_ partialPaths $ \subpath -> do
      withDisplay $ \dpy -> do
        -- 2. We get the depth of the current path
        let depth = length subpath
        -- 3. We calculate the saturation of the border
        -- This must be a value between 0 and 1
        let relativeDepth = (fromIntegral depth + 1) / (fromIntegral (length newpath') + 1)
        -- let pixel = createPixel (RGB 255 0 0)
        -- run `setWindowBorder` for each window the the subtree spanned by the subpath
        let subtree = cut subpath tree
        case subtree of
          -- if the subpath actually leads to a subtree, we use the Foldable
          -- instance of our tree to run `setWindowBorder` for each window in the subtree
          Just t -> forM_ (zip [1 ..] windows) $ \(i, w) -> io $ setWindowBorder dpy (window w) (pixel i)
            where
              windows = (foldr (:) [] t)
              n = length windows
              -- \| value between 0 and 255
              hue' :: Int -> Double
              hue' i = (fromIntegral i / fromIntegral n) * 255
              -- TODO: Use HSL instead of HSV
              pixel :: Int -> Pixel
              pixel i = createPixel $ round <$> hsv (hue' i) (1 :: Double) (255 * relativeDepth * 0.75 :: Double)
          Nothing -> return ()
    -- write the tree to a file for debugging
    return $! (rects, Just (TreeLayout tree'' defaultBranch newpath' focused))

  -- \| This function is used to modify the layout using the keyboard.
  -- \| More generally, we actually modify the layout using "messages".
  -- \| Most of the time, a message is send by pressing a key combination though, like so:
  -- \|
  -- \| > ((modMask, xK_r), sendMessage $ Rotate)
  handleMessage (TreeLayout tree defaultBranch currentPath lastFocused) someMessage
    | Just Rotate <- fromMessage someMessage = do
        -- \| 1. Rotate the branch at the given path
        -- \| We define a helper function [rotatetree]
        -- \| to rotate the branches counter-clockwise
        let tree' = apply (initDef [] currentPath) rotatetree tree
        -- \| 2. Return the new layout
        return $ Just $ TreeLayout (fromMaybe tree tree') defaultBranch currentPath lastFocused
    | Just Swap <- fromMessage someMessage = do
        -- \| 1. Swap the branch at the given path
        xmessage $ format "path: {0},\ntree: {1}" [(show currentPath), (show tree)]
        -- let tree' = apply currentPath swap tree
        -- \| 2. Return the new layout
        return $ Just $ TreeLayout tree defaultBranch currentPath lastFocused
    | Just SelectNode <- fromMessage someMessage = do
        -- \| 1. Swap the branch at the given path
        -- \| This means, we want to transform a `Leaf a` into a `Branch defaultBranch [Leaf a]`
        -- \| And we want to transform a `Branch defaultBranch [Leaf a]` into a `Leaf a`
        let tree' =
              if null currentPath
                then tree
                else tree `fromMaybe` apply currentPath (swaptree defaultBranch) tree
        let currentPath' = currentPath ++ [0]
        -- let currentPath' = case cut currentPath tree of
        --       Just (Leaf _) -> init currentPath ++ [0]
        --       _ -> initDef [] currentPath
        -- \| 2. Return the new layout
        return $ Just $ TreeLayout tree' defaultBranch currentPath' lastFocused
    | otherwise = return Nothing
    where
      -- \| helper functions
      -- \|
      -- \| Rotate a tree counter clockwise
      rotatetree :: Tree BranchNode WindowNode -> Tree BranchNode WindowNode
      rotatetree t = first (\w -> BranchNode (rotateCCW $ rotation w)) t
      -- \| Swap a tree
      swaptree :: BranchNode -> Tree BranchNode WindowNode -> Tree BranchNode WindowNode
      -- swaptree defaultBranch (Leaf w) = Branch defaultBranch [Leaf w]
      -- swaptree defaultBranch (Branch _ [Leaf w]) = Leaf w
      swaptree d b = Branch d [b]

-- | This is the starting point of our layout.
-- | You want to include this in your [layoutHook].
emptyTreeLayout :: TreeLayout Window
emptyTreeLayout =
  TreeLayout
    { tree = emptyTree,
      defaultBranch = BranchNode RIGHT,
      currentPath = [],
      lastFocused = Nothing
    }
  where
    emptyTree = Branch (BranchNode RIGHT) []

-- | Given a path, find the sum of all percentages of the windows in that tree.
percentages :: Path -> Tree BranchNode WindowNode -> Maybe Rational
percentages p t = cut p t >>= Just . sum . fmap percentage

-- | Modify the percentages of all WindowNodes at a given path by a given factor.
applyPercentages :: Rational -> Path -> Tree BranchNode WindowNode -> Maybe (Tree BranchNode WindowNode)
applyPercentages factor p t = cut p t >>= Just . fmap (\w -> w {percentage = percentage w * factor})

-- | Given a [TreeLayout], we want to make all the percentages in the tree add up to 1.
-- | If the percentages do not add up to 1, we will fix that using the following algorithm:
fixGlobalPercentages :: Tree BranchNode WindowNode -> Tree BranchNode WindowNode
fixGlobalPercentages t = normalize <$> t
  where
    totalPercentage :: Rational
    totalPercentage = sum $ percentage <$> t
    normalize :: WindowNode -> WindowNode
    normalize w = w {percentage = (percentage w) / totalPercentage}

-- | Calculate the positions of all the windows using the given [TreeLayout].
-- | The algorithm used to calculate the positions is pretty simple:
-- |
-- | 1. We are always given a [Rectangle] to work with.
-- |    This rectangle represents the dimensions the area we want to fill with
-- |    our windows.
-- | 2. We are also given a [Tree] of [WindowNode]s.
-- |    This tree represents the windows we want to place in the given [Rectangle].
-- |
-- | If we are given a [Leaf] we simply place the window in the given [Rectangle]
-- | and fill the entire area with it.
-- |
-- | If we are given a [Branch], we remember that it just consists of a list of
-- | sub[Tree]s and some [BranchNode] data. We use the [BranchNode] data to
-- | determine the direction of the split and then we split the [Rectangle] in
-- | parts in that direction. We then recursively call [layoutRects] on each
-- | sub[Tree] and each part of the [Rectangle] and combine the results.
layoutRects :: Tree BranchNode WindowNode -> Rectangle -> [(Window, Rectangle)]
layoutRects (Leaf wn) (Rectangle sx sy sw sh) = [(window wn, Rectangle sx sy sw sh)]
layoutRects (Branch (BranchNode RIGHT) bs) r@(Rectangle sx sy sw sh) = result
  where
    -- \| We need to calculate some numbers before we can construct the
    -- \| rectangle/window pairing
    -- \|
    -- \| 1. Number of subtrees in the branch
    n = length bs
    -- \| 2. The width given for each subtree
    -- \| 2a. First, sum of all the percentages of the subtrees
    subtreePercentages :: [Rational]
    subtreePercentages = sum . fmap percentage <$> bs
    -- \| 2b. Then, find the total percentage of this branch
    totalPercentage :: Rational
    totalPercentage = sum subtreePercentages
    -- \| 2c. Then, calculate the width of each subtree
    width :: Int -> Rational
    width i = (fromIntegral sw / totalPercentage) * subtreePercentages !! i
    -- \| 3. A x-offset for each subtree
    -- \| This is just the sum of the widths of all the subtrees to the left of the current subtree
    delta :: Int -> Position
    delta i = round $ sum $ width <$> [0 .. i - 1]
    -- \| 4. Define a helper function that gives us the i-th rectangle
    ithRect :: Rectangle -> Int -> Rectangle
    ithRect (Rectangle x' y' w' h') i = Rectangle (x' + delta i) y' (floor $ width i) h'
    -- \| 5. Apply [layoutRects] to each subtree
    subRects = [layoutRects subtree (ithRect r i) | (i, subtree) <- zip [0 ..] bs]
    -- \| 7. Lastly, we only need to flatten the list of lists of lists ... of rectangles
    -- \| into a list of rectangles
    result :: [(Window, Rectangle)]
    result = concat subRects
-- FIXME: This currently causes XMonad to crash
layoutRects b@(Branch (BranchNode LEFT) _) r = layoutRects (Branch (BranchNode RIGHT) (trunk $ involution b)) r
layoutRects (Branch (BranchNode UP) bs) r@(Rectangle sx sy sw sh) = result
  where
    -- \| Just like in the [RIGHT] case, we need to do the same thing here,
    -- \| but now we need to split the [Rectangle] vertically instead of
    -- \| horizontally.
    n = length bs
    subtreePercentages :: [Rational]
    subtreePercentages = sum . fmap percentage <$> bs
    totalPercentage :: Rational
    totalPercentage = sum subtreePercentages
    height :: Int -> Rational
    height i = (fromIntegral sh / totalPercentage) * subtreePercentages !! i
    delta :: Int -> Position
    delta i = round $ sum $ height <$> [0 .. i - 1]
    ithRect :: Rectangle -> Int -> Rectangle
    ithRect (Rectangle x' y' w' h') i = Rectangle x' (y' + delta i) w' (floor $ height i)
    subRects = [layoutRects subtree (ithRect r i) | (i, subtree) <- zip [0 ..] bs]
    result :: [(Window, Rectangle)]
    result = concat subRects
layoutRects b@(Branch (BranchNode DOWN) _) r = layoutRects (Branch (BranchNode UP) (trunk $ involution b)) r

-- | Simple function to create a pixel from RGB values
-- |
-- | Each value should be in the range [0, 255]
createPixel :: RGB Int -> Pixel
createPixel (RGB r g b) = fromIntegral (r `shiftL` 16 .|. g `shiftL` 8 .|. b)