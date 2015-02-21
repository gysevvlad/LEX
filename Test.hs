module Test where

import Data.Tree
import MyData.Tree
import qualified MyData.BinTree as BT

test :: (Show a, Ord a) => [a] -> IO ()
test = putStr . drawTree . mapForDT show . snd . toNumTree . (\x -> (1, x)) . BT.castToDT id . BT.fromList

