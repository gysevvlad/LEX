module Lexer where

import Data.List.Split
import Data.Tree
import Data.List

import MyData.Tree

type Production = (String, [[String]])
type Grammar = [Production]
type TreeNum a = Tree (Int, a)
type ForestNum a = [Forest (Int, a)]

type AST = Tree String

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

is :: Grammar -> (Bool, String) -> String -> (Bool, String)
is gr (ch, str) pr
	| l == '\'' && r == '\'' = 
		if base `isPrefixOf` str
		then (True, tail str) 
		else (False, str)
	| otherwise = 
		case find ( (==) pr . fst) gr of
			Just (_, defs) -> checkDefs gr (ch, str) defs
			Nothing -> error "unknown productions"
	where 
		l = head pr
		r = last pr
		base = (tail . init) pr

checkDefs :: Grammar -> (Bool, String) -> [[String]] -> (Bool, String)
checkDefs gr (ch, str) [] = (False, str)
checkDefs gr (ch, str) (def:defs) = let st@(ch', str') = checkDef gr (ch, str) def in
	if (ch' == True)
	then st
	else checkDefs gr (ch, str) defs

checkDef :: Grammar -> (Bool, String) -> [String] -> (Bool, String)
checkDef gr (ch, str) (def) = foldl (checkPr gr) (ch, str) def

checkPr :: Grammar -> (Bool, String) -> String -> (Bool, String)
checkPr gr st@(ch, str) pr
	| ch == False = st
	| ch == True = is gr st pr
{-
createAST :: Grammar -> (Bool, String) -> String -> (String, Maybe AST)
createAST gr (ch, str) pr
	| l == '\'' && r == '\'' = 
		if base `isPrefixOf` str
		then (tail str, Just $ Node base [])
		else (str, Nothing)
	| otherwise = 
		case find ( (==) pr . fst) gr of
			Just (_, defs) -> createkDefs gr (ch, str) defs
			Nothing -> error "unknown productions"
	where 
		l = head pr
		r = last pr
		base = (tail . init) pr

checkDefs :: Grammar -> (Bool, String) -> [[String]] -> (Bool, String)
checkDefs gr (ch, str) [] = (False, str)
checkDefs gr (ch, str) (def:defs) = let st@(ch', str') = checkDef gr (ch, str) def in
	if (ch' == True)
	then st
	else checkDefs gr (ch, str) defs

checkDef :: Grammar -> (Bool, String) -> [String] -> (Bool, String)
checkDef gr (ch, str) (def) = foldl (checkPr gr) (ch, str) def

createDef :: Grammar -> [String] -> String -> (String, Maybe AST)
createDef gr def@(pr:prs) str = 
-}
{-
createPr :: Grammar -> String -> String -> (String, Maybe AST)
createPr gr pr str
	| l == '\'' && r == '\'' = 
		if base `isPrefixOf` str
		then (tail str, Just $ Node base [])
		else (str, Nothing)
	| otherwise =
		case find ( (==) pr . fst) gr of
			Just (_, defs) -> 
				let (str', ast) = createkDefs gr defs str in

					then (str, Nothing)
					else (str', Just $ Node pr asts)
			Nothing -> (str, Nothing)
	where
		l = head pr
		r = last pr
		base = (tail . init) pr
		
createkDefs :: Grammar -> [[String]] -> String -> (String, Maybe AST)
createkDefs gr defs str 

test gr = createPr (readGrammar gr)
-}
sup1 =  concatMap forElem .grammarToForest .readGrammar
	where forElem = (++) "\n" .drawTree .mapForDT show .snd .toNumTree .(\x -> (1, x))

main file = do
	str <- readFile file
	putStr $ sup1 str

main2 file pr = do
	f <- readFile file
	str <- getLine
	print $ is (readGrammar f) (True, str) pr


