module MyData.Tree where

import Data.Tree(Tree(Node), Forest)

type NumTree a = Tree (Int, a)
type NumForest a = [NumTree a]

toNumTree :: (Int, Tree a) -> (Int, NumTree a)
toNumTree (n, Node x []) = (n+1, Node (n, x) [])
toNumTree (n, Node x xs) = let (n', xs') = convertForest (n+1) xs in (n', Node (n, x) xs')

convertForest :: Int -> Forest a -> (Int, NumForest a)
convertForest n xs = foldl addTree (n, []) xs

addTree :: (Int, NumForest a) -> Tree a -> (Int, NumForest a)
addTree (n, nodes) node = let (n', node') = toNumTree (n, node) in (n', node':nodes)

mapForDT :: (a -> b) -> Tree a -> Tree b
mapForDT f (Node x nodes) = Node (f x) (foldl (\acc node -> (mapForDT f node):acc) [] nodes)

-- test = (putStr . drawTree . mapForDT show . snd . toNumTree . (\x -> (1, x)) . BT.castToDT id . BT.fromList ) ["asd","rtey","wertwe","erwtv","f","r"]

