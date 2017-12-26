module Search where

import Control.Lens (makeLenses, (^.), view, use, (.=), (+=))
import Control.Monad.Logger (logDebug)
import Control.Monad.State (execState)
import Control.Monad ((>=>))
import System.Random.MWC (withSystemRandom, asGenIO)
import System.Random.MWC.Distributions (categorical)
import GHC.Float (float2Double)
import Data.Maybe (fromJust, catMaybes)
import Data.Function (on)
import Data.Tree
import Data.List (maximumBy)
import qualified Data.Tree.Zipper as T
import Data.Tree.Zipper (TreePos, Full, Empty, label, parent, children, tree, childAt)
import qualified Data.Vector as V
import RuleSet
import Board
import Prior
import Play

import Debug.Trace

data Label = Label { _lbl_N :: Int, _lbl_W :: Float
                   , _lbl_Q, _lbl_P :: Float
                   , _lbl_brd :: Board
                   , _lbl_num_capture  :: Int  -- number of captures in this turn
                   , _lbl_player_black :: Bool -- this is black's turn
                   , _lbl_player_pass  :: Bool -- this is a pass step 
                   }
makeLenses ''Label

type Node = TreePos Full Label
type NodeWithHole = TreePos Empty Label

initial:: Int -> Node
initial board_size = T.fromTree (Node lbl [])
  where
    lbl = Label { _lbl_N = 0, _lbl_W = 0
                , _lbl_Q = 0, _lbl_P = 0
                , _lbl_brd = emptyBoard (BoardConf {_cfg_size = board_size})
                , _lbl_num_capture  = 0
                , _lbl_player_black = False
                , _lbl_player_pass  = False }

moves :: Label -> Stone -> Play [(Board, Float, Int, Bool)]
moves lbl next = do
  allow_suicide <- view (play_ruleset . ruleset_allow_suicide)
  epi <- view (play_hyper_parms . parm_epsilon)
  (pp, _) <- predicateWithDirichletNoise brd (lbl ^. lbl_player_black) epi
  let step pos = case move brd pos next of 
                   (b1, cp, suicide) -> if allow_suicide || not suicide
                                          then Just (b1, prOfPos pp pos, cp, next == Black)
                                          else Nothing
      put_moves = catMaybes $ map step all_pos
      pass_move = (brd, prOfPass pp, 0, next == Black)
  return $ pass_move : put_moves
  where
    brd = lbl ^. lbl_brd
    board_size = brd ^. board_cfg . cfg_size
    -- valid move point is empty
    -- TODO is it valid to fill in an eye of myself?
    valid p = get brd p == Empty
    all_pos = [(x,y) | x <- [1..board_size], y <- [1..board_size], valid (x, y)]
        
-- expand a node
-- note: if the node is expaned already, it is untouched 
-- returns the node with children being all the possible moves
expand :: Node -> Play Node
expand node | T.hasChildren node = return node 
            | otherwise = do let lbl  = label node
                                 b0   = lbl ^. lbl_brd
                                 next = if lbl ^. lbl_player_black then White else Black
                             -- extend with all valid play of stone
                             all_moves <- moves lbl next
                             let n0 = foldr mkNode (children node) all_moves
                             -- extend with a pass
                             let n1 = T.insert (Node (mkLabel 0 b0 0 (next == Black) True) []) n0
                             return $ fromJust $ parent n1
  where 
    mkLabel prior board captures black pass = 
      Label { _lbl_N = 0, _lbl_W = 0, 
              _lbl_Q = 0, _lbl_P = prior,
              _lbl_brd = board, 
              _lbl_num_capture = captures,
              _lbl_player_black = black,
              _lbl_player_pass = pass }
    mkNode (b, p, c, x) n = 
      let lbl = mkLabel p b c x False
      in  T.nextSpace $ T.insert (Node lbl []) n

descend :: Node -> Play Node
descend root = do
  (cnt, leaf) <- walkDown 0 root
  (_, reward) <- io $ predicate (label leaf ^. lbl_brd) (label leaf ^. lbl_player_black)
  walkUp cnt reward leaf

  where
    walkDown cnt node = do
      hyper_parms <- view play_hyper_parms
      node_expended <- expand node
      if not (T.hasChildren node_expended)
        then 
          return (cnt, node_expended)
        else
          let chl = lblChildren node_expended
              next_node = fromJust $ childAt (imaxBy (priority hyper_parms chl) chl) node_expended
          in case () of
            -- two consecutive pass means end of game
            _ | label node ^. lbl_player_pass && label next_node ^. lbl_player_pass -> 
              do $(logDebug) "two passes"
                 return (cnt+1, next_node)
            -- terminate too long descending 
            _ | cnt > (hyper_parms ^. parm_descend_threshold) -> 
              do $(logDebug) "too long descending steps"
                 return (cnt+1, next_node)
            _ -> walkDown (cnt+1) $ next_node

    walkUp 0 _ node = return node
    walkUp cnt reward node = 
      walkUp (cnt-1) (-reward) $ fromJust $ parent $ T.modifyLabel (execState $ do 
        lbl_N += 1
        lbl_W += reward
        w <- use lbl_W
        n <- use lbl_N
        lbl_Q .= w / fromIntegral n) node

selfPlayStep :: Node -> Play Node
selfPlayStep root = do
  allowresign <- view play_allow_resign
  parms <- view play_hyper_parms
  let num_desc = parms ^. parm_number_descends
      rev_temp = 1 / parms ^. parm_temperature
      resign_threshold = parms ^. parm_resign_threshold
  
  root <- foldr (>=>) return (replicate num_desc descend) root
  traceShowM (_lbl_brd $ label root)
  let steps = V.fromList (lblChildren root)
  if allowresign && V.all ((< resign_threshold) . (^. lbl_Q)) steps
    then -- resign
      return root
    else do
      let weights = V.map ((**rev_temp) . fromIntegral . (^. lbl_N)) steps
          wsum = V.sum weights
          dist = V.map (float2Double . (/wsum)) weights
      step_idx <- io $ withSystemRandom . asGenIO $ categorical dist
      let chosen_step = fromJust $ childAt step_idx root
      play_number_steps += 1
      if label chosen_step ^. lbl_player_black
        then play_prisoners_white += label chosen_step ^. lbl_num_capture
        else play_prisoners_black += label chosen_step ^. lbl_num_capture
      return chosen_step

priority :: HyperParms -> [Label] -> Label -> Float
priority parms allLbls thisLbl = 
  let sumOfN = fromIntegral $ sum $ map (^. lbl_N) allLbls
      cpuct = parms ^. parm_constant_puct
  in thisLbl ^. lbl_Q + cpuct * (thisLbl ^. lbl_P) * sqrt(sumOfN) / (fromIntegral $ 1 + thisLbl ^. lbl_N)

lblChildren :: Node -> [Label]
lblChildren = map rootLabel . subForest . tree
  
imaxBy :: Ord t => (a -> t) -> [a] -> Int
imaxBy cmpfunc = fst . maximumBy (compare `on` cmpfunc . snd) . zip [0..]