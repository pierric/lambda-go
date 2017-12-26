module Prior where

import Control.Lens (makeLenses, (^.), (%~), (&))
import qualified Data.Vector as V
import Data.Vector ((!))
import System.Random.MWC (withSystemRandom, asGenIO)
import System.Random.MWC.Distributions (dirichlet)
import GHC.Float (double2Float)
import Board
import Play

-- PriorProb is a vector if size (board_size ^ 2 + 1),
-- the prob at index 0 is for the pass move.
data PriorProb = PriorProb {
    _pp_prob :: V.Vector Float,
    _pp_board_size :: Int
}
makeLenses ''PriorProb

prOfPos :: PriorProb -> Pos -> Float
prOfPos pp (x,y) = (pp ^. pp_prob) ! ((y-1) * pp ^. pp_board_size + x)

prOfPass :: PriorProb -> Float
prOfPass pp = (pp ^. pp_prob) ! 0

predicate :: Board -> Bool -> Play (PriorProb, Float)
predicate board black = do
    let sz = board ^. board_cfg . cfg_size
        nn = sz * sz + 1
        dist = V.fromList(replicate nn (1 / fromIntegral nn))
    return (PriorProb { _pp_prob = dist, _pp_board_size = sz}, 0)

predicateWithDirichletNoise :: Board -> Bool -> Float -> Play (PriorProb, Float)
predicateWithDirichletNoise board black epsilon = do
    (p1, v) <- predicate board black
    let sz = board ^. board_cfg . cfg_size
    p2 <- io $ withSystemRandom . asGenIO $ dirichlet (V.replicate (sz * sz + 1) 0.03)
    let ea = epsilon
        eb = 1 - epsilon
    let p3 = p1 & pp_prob %~ V.zipWith (\a b -> ea * double2Float a + eb * b) p2
    return (p3, v)