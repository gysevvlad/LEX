module MyData.BinTree where

import Prelude(Bool(..), Ord(..), Eq(..), Show(..), String, foldr)
import qualified Data.Tree as DT(Tree(Node))


data BinTree a = EmptyTree | Node a (BinTree a) (BinTree a)
	deriving(Show)

singleton :: a -> BinTree a
singleton x = Node x EmptyTree EmptyTree

insert :: (Ord a) => a -> BinTree a -> BinTree a
insert x EmptyTree = singleton x
insert x (Node a l r)
	| x == a = Node x l r
	| x < a = Node a (insert x l) r
	| x > a = Node a l (insert x r)

elem :: (Ord a) => a -> BinTree a -> Bool
elem x EmptyTree = False
elem x (Node a l r)
	| x == a = True
	| x < a = elem x l
	| x > a = elem x r
 
fromList :: (Ord a) => [a] -> BinTree a
fromList xs = foldr insert EmptyTree xs 

map :: (a -> b) -> BinTree a -> BinTree b
map f EmptyTree = EmptyTree
map f (Node x l r) = Node (f x) (map f l) (map f r)

castToDT :: (a -> b) -> BinTree a -> DT.Tree b
castToDT f (Node x EmptyTree EmptyTree) = DT.Node (f x) []
castToDT f (Node x EmptyTree r) = DT.Node (f x) ((castToDT f r):[])
castToDT f (Node x l EmptyTree) = DT.Node (f x) ((castToDT f l):[])
castToDT f (Node x l r) = DT.Node (f x) ((castToDT f l):(castToDT f r):[])



