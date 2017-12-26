module RuleSet where 

import Control.Lens (makeLenses, (^.), (%~), _1, _2)
import qualified Data.Vector as V
import Data.List (foldl')
import Board

data RuleSet = RuleSet {
    _ruleset_allow_suicide :: Bool, 
    _ruleset_scoring :: Int -> Int -> Int -> Board -> (Int, Int)
}
makeLenses ''RuleSet

chinese :: RuleSet
chinese = RuleSet {
    _ruleset_allow_suicide = True,
    _ruleset_scoring = scoringChinese
}
-- scoring with Chinese rule
-- returns the size of area owned by black and white
-- Note:
-- + clean up the board, removing all the dead stones
-- + fill in the area owned by black and while
-- + count the area
scoringChinese :: Int -> Int -> Int -> Board -> (Int, Int)
scoringChinese _ _ _ b0 = stat (b2 ^. board_data)
  where
    cfg  = b0 ^. board_cfg
    size = cfg ^. cfg_size
    (b1, _, _) = clean b0
    b2 = foldl' step b1 [0..size*size-1]
    step bt i
      | get bt pos /= Empty = bt
      | blackarea = floodFill pos Black bt ^. _1
      | whitearea = floodFill pos White bt ^. _1
      | otherwise = bt
      where
        pos  = index2Coord cfg i
        (bt'1, _, bdr) = floodFill pos Hash bt
        ss = map (get bt'1) bdr
        blackarea = all (==Black) ss
        whitearea = all (==White) ss
    stat dat = V.foldr (\x -> case x of Black -> _1 %~ (+1); White -> _2 %~ (+1); _ -> id) (0, 0) dat