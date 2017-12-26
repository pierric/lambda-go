module Main where

import Data.Tree.Zipper
import Board (Board)

data Label = Label {
    _board   :: Board,
    _v, _w   :: Float,
    _pv, _pw :: Float
}

type Node = TreePos Full Label


main = return ()