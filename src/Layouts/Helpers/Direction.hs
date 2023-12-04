module Layouts.Helpers.Direction where

-- | A datatype to represent the direction of a split in a [Node] of our [TreeLayout
-- |
-- | Example:
-- |
-- | Right:
-- | [window1] [window2] [window3] [window4]
-- |
-- | Left:
-- | [window4] [window3] [window2] [window1]
-- |
-- | Up:
-- | [window4]
-- | [window3]
-- | [window2]
-- | [window1]
-- |
-- | Down:
-- | [window1]
-- | [window2]
-- | [window3]
-- | [window4]
data Direction = RIGHT | LEFT | UP | DOWN
  deriving (Show, Read, Eq)

-- | Rotation function for [Direction]s representing a counter-clockwise rotation.
rotateCCW :: Direction -> Direction
rotateCCW RIGHT = UP
rotateCCW UP = RIGHT
rotateCCW LEFT = UP
rotateCCW DOWN = RIGHT

-- rotateCCW UP = LEFT
-- rotateCCW LEFT = DOWN
-- rotateCCW DOWN = RIGHT

-- | Rotation function for [Direction]s representing a clockwise rotation.
rotateCW :: Direction -> Direction
rotateCW RIGHT = DOWN
rotateCW DOWN = LEFT
rotateCW LEFT = UP
rotateCW UP = RIGHT