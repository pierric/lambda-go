module Main where

import Test.Hspec
import Test.QuickCheck
import qualified Data.Vector as V
import Board

main = hspec $ do
  describe "test floodFill" $ do
    it "1" $ let b1 = makeBoardOfAll 3 White
                 (b2, n, _) = floodFill (1,1) Black b1
             in (b2 `shouldBe` makeBoardOfAll 3 Black) >> (n `shouldBe` 9)
    it "2" $ let b1 = makeBoardOfAll 3 Black
                 (b2, n, _) = floodFill (1,1) White b1
             in (b2 `shouldBe` makeBoardOfAll 3 White) >> (n `shouldBe` 9)
    it "3" $ let b1 = makeBoardOfAll 3 Empty
                 (b2, n, _) = floodFill (1,1) Black b1
             in (b2 `shouldBe` makeBoardOfAll 3 Black) >> (n `shouldBe` 9)
    it "4" $ let b1 = makeBoardOfAll 3 White
                 (b2, n, _) = floodFill (1,1) Empty b1
             in (b2 `shouldBe` makeBoardOfAll 3 Empty) >> (n `shouldBe` 9)
    it "5" $ let b1 = makeBoard ("---" ++
                                 "---" ++
                                 "+++")
                 (b2, n, _) = floodFill (3,2) Black b1
             in (b2 `shouldBe` makeBoardOfAll 3 Black) >> (n `shouldBe` 6)
    it "6" $ let b1 = makeBoard ("---" ++
                                 "---" ++
                                 "+-+")
                 (b2, n, _) = floodFill (3,1) Black b1
             in (b2 `shouldBe` makeBoardOfAll 3 Black) >> (n `shouldBe` 7)
    it "7" $ let b1 = makeBoard ("--." ++
                                 ".--" ++
                                 ".-+")
                 (b2, n, _) = floodFill (1,1) Black b1
                 b3 = makeBoard ("++." ++
                                 ".++" ++
                                 ".++")
             in (b2 `shouldBe` b3) >> (n `shouldBe` 5)
    it "8" $ let b1 = makeBoard ("--." ++
                                 ".+-" ++
                                 ".-+")
                 (b2, n, _) = floodFill (1,1) Black b1
                 b3 = makeBoard ("++." ++
                                 ".+-" ++
                                 ".-+")
             in (b2 `shouldBe` b3) >> (n `shouldBe` 2)
    it "9" $ let b1 = makeBoard ("--." ++
                                 ".+-" ++
                                 ".-+")
                 (b2, n, _) = floodFill (1,3) Black b1
                 b3 = makeBoard ("--." ++
                                 "++-" ++
                                 "+-+")
             in (b2 `shouldBe` b3) >> (n `shouldBe` 2)
    it "10"$ let b1 = makeBoard (".+." ++
                                 "-.-" ++
                                 ".+.")
                 (b2, n, _) = floodFill (2,2) Black b1
                 b3 = makeBoard (".+." ++
                                 "-+-" ++
                                 ".+.")
             in (b2 `shouldBe` b3) >> (n `shouldBe` 1)
    it "11"$ let b1 = makeBoard ("..." ++
                                 ".+." ++
                                 "+.+")
                 (b2, n, _) = floodFill (2,3) Black b1
                 b3 = makeBoard ("..." ++
                                 ".+." ++
                                 "+++")
             in (b2 `shouldBe` b3) >> (n `shouldBe` 1)
    it "12"$ let b1 = makeBoard ("..." ++
                                 ".+." ++
                                 "...")
                 (b2, n, _) = floodFill (2,2) Black b1
             in (b2 `shouldBe` b1) >> (n `shouldBe` 0)
    it "13"$ let b1 = makeBoard ("..." ++
                                 ".-." ++
                                 "...")
                 (b2, n, _) = floodFill (2,2) Black b1
                 b3 = makeBoard ("..." ++
                                 ".+." ++
                                 "...")                 
             in (b2 `shouldBe` b3) >> (n `shouldBe` 1)
  describe "capture" $ do
    it "1" $ let b1 = makeBoard ("+-." ++
                                 "-.." ++ 
                                 "...")
                 (b2, n) = capture b1 (1,1)
                 b3 = makeBoard (".-." ++
                                 "-.." ++ 
                                 "...")
             in (b2 `shouldBe` b3) >> (n `shouldBe` 1)
    it "2" $ let b1 = makeBoard (".+-" ++
                                 ".++" ++ 
                                 "...")
                 (b2, n) = capture b1 (3,1)
                 b3 = makeBoard (".+." ++
                                 ".++" ++ 
                                 "...")
             in (b2 `shouldBe` b3) >> (n `shouldBe` 1)
    it "3" $ let b1 = makeBoard (".+." ++
                                 "+++" ++ 
                                 "-+.")
                 (b2, n) = capture b1 (1,3)
                 b3 = makeBoard (".+." ++
                                 "+++" ++ 
                                 ".+.")
             in (b2 `shouldBe` b3) >> (n `shouldBe` 1)
    it "4" $ let b1 = makeBoard ("..." ++
                                 "---" ++ 
                                 "--+")
                 (b2, n) = capture b1 (3,3)
                 b3 = makeBoard ("..." ++
                                 "---" ++ 
                                 "--.")
             in (b2 `shouldBe` b3) >> (n `shouldBe` 1)
    it "5" $ let b1 = makeBoard ("+++" ++
                                 "++-" ++ 
                                 "---")
                 (b2, n) = capture b1 (2,1)
                 b3 = makeBoard ("..." ++
                                 "..-" ++ 
                                 "---")
             in (b2 `shouldBe` b3) >> (n `shouldBe` 5)
    it "6" $ let b1 = makeBoard ("+++" ++
                                 "++-" ++ 
                                 "---")
                 (b2, n) = capture b1 (2,3)
                 b3 = makeBoard ("+++" ++
                                 "++." ++ 
                                 "...")
             in (b2 `shouldBe` b3) >> (n `shouldBe` 4)
    it "7" $ let b1 = makeBoard ("+++" ++
                                 "+-+" ++ 
                                 "+++")
                 (b2, n) = capture b1 (1,1)
                 b3 = makeBoard ("..." ++
                                 ".-." ++ 
                                 "...")
             in (b2 `shouldBe` b3) >> (n `shouldBe` 8)
    it "8" $ let b1 = makeBoard ("+++" ++
                                 "+-+" ++ 
                                 "+++")
                 (b2, n) = capture b1 (2,2)
                 b3 = makeBoard ("+++" ++
                                 "+.+" ++ 
                                 "+++")
             in (b2 `shouldBe` b3) >> (n `shouldBe` 1)
  describe "move (normal)" $ do
    it "1" $ let b1 = makeBoard ("..." ++
                                 "..." ++
                                 "...")
                 (b2, n, d) = move b1 (1,1) White
                 b3 = makeBoard ("-.." ++
                                 "..." ++
                                 "...")
             in (b2 `shouldBe` b3) >> (n `shouldBe` 0) >> (d `shouldBe` False)
    it "2" $ let b1 = makeBoard ("..." ++
                                 "..." ++
                                 "...")
                 (b2, n, d) = move b1 (2,2) White
                 b3 = makeBoard ("..." ++
                                 ".-." ++
                                 "...")
             in (b2 `shouldBe` b3) >> (n `shouldBe` 0) >> (d `shouldBe` False)
  describe "move (suicide)" $ do
    it "1" $ let b1 = makeBoard (".-." ++
                                 "-.." ++
                                 "...")
                 (b2, n, d) = move b1 (1,1) Black
             in (b2 `shouldBe` b1) >> (n `shouldBe` 0) >> (d `shouldBe` True)
    it "2" $ let b1 = makeBoard (".-." ++
                                 "-.-" ++
                                 ".-.")
                 (b2, n, d) = move b1 (2,2) Black
             in (b2 `shouldBe` b1) >> (n `shouldBe` 0) >> (d `shouldBe` True)
    it "3" $ let b1 = makeBoard ("..." ++
                                 "..-" ++
                                 ".-.")
                 (b2, n, d) = move b1 (3,3) Black
             in (b2 `shouldBe` b1) >> (n `shouldBe` 0) >> (d `shouldBe` True)
    it "4" $ let b1 = makeBoard ("..." ++
                                 "-.." ++
                                 ".-.")
                 (b2, n, d) = move b1 (1,3) Black
             in (b2 `shouldBe` b1) >> (n `shouldBe` 0) >> (d `shouldBe` True)
    it "5" $ let b1 = makeBoard (".-." ++
                                 "..-" ++
                                 "...")
                 (b2, n, d) = move b1 (3,1) Black
             in (b2 `shouldBe` b1) >> (n `shouldBe` 0) >> (d `shouldBe` True)
    it "6" $ let b1 = makeBoard ("+++" ++
                                 "+--" ++
                                 "+-.")
                 (b2, n, d) = move b1 (3,3) White
                 b3 = makeBoard ("+++" ++
                                 "+.." ++
                                 "+..")
             in (b2 `shouldBe` b3) >> (n `shouldBe` 0) >> (d `shouldBe` True)
    it "7" $ let b1 = makeBoard (".++" ++
                                 "+--" ++
                                 "+--")
                 (b2, n, d) = move b1 (1,1) Black
                 b3 = makeBoard ("..." ++
                                 ".--" ++
                                 ".--")
             in (b2 `shouldBe` b3) >> (n `shouldBe` 0) >> (d `shouldBe` True)             
             
-- create a board from string
-- . for empty
-- X for wall
-- + for black
-- - for white
-- # for hash
-- string should be of N*N symbols
makeBoard :: String -> Board
makeBoard spec = Board { _board_cfg = cfg, _board_data = dat } 
  where
    len = length spec
    sz  = floor $ sqrt $ fromIntegral len
    cfg = BoardConf { _cfg_size = sz }
    dat = V.fromList $ wall ++ go spec ++ wall ++ [Wall]
    wall  = replicate (sz + 1) Wall
    go [] = []
    go xs = (Wall : map symbol (take sz xs)) ++ go (drop sz xs)
    symbol '.' = Empty
    symbol 'X' = Wall
    symbol '+' = Black
    symbol '-' = White
    symbol '#' = Hash

makeBoardOfAll :: Int -> Stone -> Board
makeBoardOfAll sz st = makeBoard $ concat $ replicate (sz * sz) $ show st

deriving instance Show BoardConf
deriving instance Eq BoardConf
deriving instance Eq Board