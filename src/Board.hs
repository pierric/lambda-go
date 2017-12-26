module Board where

import Control.Lens hiding (Empty)
import Data.Tuple (swap)
import Data.List  (foldl')
import qualified Data.Vector as V
import Data.Vector ((//), (!))
import qualified Data.Vector.Mutable as V hiding (modify)
import Control.Monad (when)
import Control.Arrow (second)
import Data.STRef.Strict (newSTRef, readSTRef, modifySTRef)
import Control.Monad.ST.Strict (runST)

data Stone = White | Black | Empty | Wall | Hash
    deriving Eq

data BoardConf = BoardConf { _cfg_size :: Int }
makeLenses ''BoardConf
data Board = Board { _board_cfg :: BoardConf, _board_data :: V.Vector Stone }
makeLenses ''Board

type Pos = (Int, Int)

instance Show Stone where
    show White = "-"
    show Black = "+"
    show Empty = "."
    show Wall  = "X"
    show Hash  = "#"

instance Show Board where
    show b = unlines $ byRow $ V.toList (b ^. board_data)
      where
        byRow []  = error "malformed board"  -- should not happen
        byRow [_] = []  -- drop the last sentinel point, which is always a 'Wall'
        byRow ps = let (r0,rs) = splitAt (b ^. board_cfg . cfg_size + 1) ps
                   in concatMap show r0 : byRow rs

-- Board are stored in row-major order.
-- extra Wall are placed as row 0, row N, and column 0.
-- xxxx
-- x...
-- x...
-- x...
-- xxxx
emptyBoard :: BoardConf -> Board
emptyBoard cfg = Board cfg dat
  where
    size = (cfg ^. cfg_size + 1) * (cfg ^. cfg_size + 2) + 1
    dat = V.generate size fill
    fill n = let (x, y) = index2Coord cfg n
             in if x == 0 || y == 0 || y == cfg ^. cfg_size + 1 then Wall else Empty


coord2Index :: BoardConf -> Pos -> Int
coord2Index c (x, y) = y * (c ^. cfg_size + 1) + x
index2Coord :: BoardConf -> Int -> Pos
index2Coord c p   = swap (divMod p (c ^. cfg_size + 1))

colorSwap :: Stone -> Stone
colorSwap Black = White
colorSwap White = Black
colorSwap _     = error "unexpected call of colorSwap"

put :: Board -> Stone -> Pos -> Board
put b s p = putList b s [p]

putList :: Board -> Stone -> [Pos] -> Board
putList b s ps = let dat = b ^. board_data
                     cfg = b ^. board_cfg
                 in b & board_data .~ (dat // [(coord2Index cfg p, s) | p <- ps])

get :: Board -> Pos -> Stone
get (Board cfg dat) p = dat ! (coord2Index cfg p)

neighbours :: Pos -> [Pos]
neighbours (x, y) = [ep, sp, wp, np]
  where
    wp = (x-1,y)
    np = (x,y-1)
    ep = (x+1,y)
    sp = (x,y+1)
            
diagNeighbours :: Pos -> [Pos]
diagNeighbours (x, y) = [esp, wsp, wnp, enp]
  where
    wnp = (x-1,y-1)
    enp = (x+1,y-1)
    wsp = (x-1,y+1)
    esp = (x+1,y+1)

-- flood fill in specified position
-- returns the updated board, number of fills, and also the boundary
floodFill :: Pos -> Stone -> Board -> (Board, Int, [Pos])
floodFill p fil b = runST upd
  where
    cfg  = b ^. board_cfg
    vec0 = b ^. board_data
    upd = do
        vec <- V.thaw vec0
        cnt <- newSTRef 0
        bdr <- newSTRef []
        let pos = coord2Index cfg p
        clr <- V.unsafeRead vec pos
        when (clr /= fil) $ do
            V.unsafeWrite vec pos fil
            modifySTRef cnt (+1)
            go vec cnt bdr clr (neighbours p)
        _vec <- V.unsafeFreeze vec
        _cnt <- readSTRef cnt
        _bdr <- readSTRef bdr
        return (b & board_data .~ _vec, _cnt, _bdr)
    go _ _ _ _ [] = return ()
    go vec cnt bdr clr (p0:ps) = do
    --   | p0 ^. _1 <= 0 || p0 ^. _1 > cfg ^. cfg_size = go vec cnt bdr clr ps
    --   | p0 ^. _2 <= 0 || p0 ^. _2 > cfg ^. cfg_size = go vec cnt bdr clr ps
    --   | otherwise = do
        let pos = coord2Index cfg p0
        s0 <- V.unsafeRead vec pos
        if (s0 == clr)
            then do 
                V.unsafeWrite vec pos fil
                modifySTRef cnt (+1)
                go vec cnt bdr clr (ps ++ neighbours p0)
            else do
                modifySTRef bdr (p0:)
                go vec cnt bdr clr ps

eyeLike :: Board -> Pos -> Maybe Stone
eyeLike b p = if ptx == Empty && (pt0 == White || pt0 == Black) && all (==pt0) rest_pts
                then Just pt0
                else Nothing
  where
    ptx = get b p
    pt0:rest_pts = filter (/=Wall) $ map (get b) (neighbours p)

eye :: Board -> Pos -> Maybe Stone
eye b p = do pt <- eyeLike b p
             let pt' = colorSwap pt
                 diagconn = map (get b) (diagNeighbours p)
                 -- number of diag position occupied by opponent
                 oppo = length $ filter (==pt')  diagconn
                 -- by the wall
                 wall = any (==Wall) diagconn
              -- test if false eye
             when (wall && oppo >= 1) $ fail "not eye"
             when (oppo >= 2) $ fail "not eye"
             return pt


-- playerSwap :: Board -> Board
-- playerSwap b = b { _board_data = V.map colorSwap (_board_data b) }

-- calculate the number of liberties for the stone at given position
-- precondition: there is no Hash on the board
liberty :: Board -> Pos -> Maybe [Pos]
liberty b p = if get b p == Empty then Nothing else Just free
  where
    (_b, _, bdr) = floodFill p Hash b
    free = filter ((Empty==) . get _b) bdr


-- try to capture the stone at the specified position
-- return the input board if not capturable, 
--        the new board and number of stone captured otherwise.
capture :: Board -> Pos -> (Board, Int)
capture b p = case (liberty b p, floodFill p Empty b) of 
                (Just [], (_b, _c, _))  -> (_b, _c)
                _ -> (b, 0)

-- a single step at specified position
-- return a new board and number of capured stones
-- Note: it does not check if the position is valid to put
--       validity is rule dependent and should be verified before
--       call of move
move :: Board -> Pos -> Stone -> (Board, Int, Bool)
move b p s = (b2, num_cap, suicide)
  where 
    -- put Black stone at the specified position
    b0 = put b s p
    -- capture adjacent (transitively) stones
    -- note that any capturable stone is definitely connect to some neighbour of 
    -- this position, otherwise it should have been captured in some earlier step.
    (b1, num_cap) = let opponent = colorSwap s
                        capture_and_count (_b, _c) _p = 
                            if get _b _p == opponent 
                                then second (+_c) (capture _b _p) 
                                else (_b, _c)
                    in foldl' capture_and_count (b0, 0) (neighbours p)
    -- if no opponent is captured, then test if it is a suicide step.
    -- most rule sets disallow suicide.
    (b2, num_self_cap) = capture b1 p
    suicide = num_cap == 0 && num_self_cap > 0

-- remove all dead stones
-- return the new board, number of dead black stones, number of dead white black stones
clean :: Board -> (Board, Int, Int)
clean b = foldl' step (b, 0, 0) [0..size*size-1]
  where
    cfg  = b ^. board_cfg
    size = cfg ^. cfg_size
    step s@(b0, bc, wc) i 
        | Just [] <- nlib  = (b1, bc1, wc1)
        -- with one liberty, speculartively fill it
        | Just [x] <- nlib,
          Just []  <- liberty (put b0 ston x) x = (b1, bc1, wc1)
        | otherwise = s
      where
        pos  = index2Coord cfg i
        nlib = liberty b0 pos
        ston = get b0 pos
        (b1, n, _) = floodFill pos Empty b0 
        (bc1, wc1) = if ston == Black then (bc + n, wc) else (bc, wc + n)



