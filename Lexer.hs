module Lexer where

import Data.List.Split
import Data.Tree
import Data.List

type Production = (String, [[String]])
type Grammar = [Production]
type TreeNum a = Tree (Int, a)
type ForestNum a = [Forest (Int, a)]

readProduction :: String -> Production
readProduction = ((\(x:_:xs) -> (x, splitWhen (=="|") xs)) . words)

readGrammar :: String -> Grammar
readGrammar = ((map readProduction) . lines) 

grammarToForest :: Grammar -> Forest String
grammarToForest =
	map (\(name, next) -> 
		Node name (map (\x -> 
			Node "" (map (\y ->
				Node y []) x)) next)) 

is :: Grammar -> (String, Bool) -> String -> (String, Bool)
is gr (str, False) s = (str, False)
is gr ([], check) s = ([], False)
is gr (str, check) s
	| head s == head "'" = 
		let 
			term = (tail . init) s
			(begin, end) = splitAt (length term) str
			in 	if term == begin
				then (end, True)
				else (str, False)
	| otherwise =
		let p' = find (\(x, xs) -> s == x) gr
		in case p' of 
			Nothing		-> error "unknown productions"; 
			Just p		-> 
				let 
					m = (\x -> foldl (is gr) (str, True) x)
					m' = (map (m) (snd p)) 
					r' = find (\(_, b) -> b == True) m'
				in case (r') of
					Nothing	-> error $ show m'
					Just r	-> r

{-
main = do 
	str <- readFile "input.txt"
	(putStrLn . drawForest . grammarToForest . readGrammar) str
-}

main = is (readGrammar "digit = '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' \nint = digit int | digit ") ("1", True) "digit"

main2 = do
	str <- readFile "input.txt"
	print $ is (readGrammar str) ("1.2", True) "int"
	--print $ readGrammar str

main3 file start = do
	str <- readFile file
	instr <- getLine
	print $ is (readGrammar str) (instr, True) start



