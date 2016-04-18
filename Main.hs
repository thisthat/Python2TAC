module Main where
import Lexer
import Parser
import TypeChecker
import Abs
import Tac
import Token

main = do
	inStr <- getContents
	let lTok = (convertToken (alexScanTokens inStr) 0 True [0]) ++ [((0,0,""),End)]
	--let lTok2 = (alexScanTokens (inStr++"\n"))
	let pParsed = parseProgram ( removeDupEnd lTok)
	let errorList = checkProgram pParsed
	--putStrLn $ show lTok
	if all (=="[WARNING]") (map (take 9) errorList) || errorList == [] then do
		writeList errorList
		writeFile "PrettyProgram.py" (Abs.prettyPrintProgram pParsed)
		putStrLn "-- Generato il file PrettyProgram.py --"
		writeFile "TAC.txt" (Tac.prettyPrintTac $ Tac.createTac pParsed)
		putStrLn "-- Generato il file TAC.txt --"
	else (writeList errorList)
	putStrLn "_________________"


writeList [] = putStrLn "-- FINE --"
writeList (x:xs) = (putStrLn x) >> (writeList xs)

-- Algo
-- Se sono dopo un End controllo il numero di tab
-- Se il numero di tab > top stack => lo addo in cima allo stack e genero un OpenB e un End
-- Se il mumero di tab < top stack => cerco il numero all'interno dello stack
--		Se esiste -> genero un numero di token di CloseB pari al numero di pop di stack che faccio per arrivare a lui
--		Se non Esiste -> errore indentazione non corretta
-- Quando arrivo alla fine svuoto lo stack generando (n-1) CloseB e un End terminale se già non esiste

convertToken [] counter end stack 
	| (length stack) /= 1 = (genNewList ( (length stack) - 1) ((0,0,"}"),CloseB))
	| otherwise = []
convertToken l@(x:xs) counter end stack = list
	where
		listSenzaTab = removeTab l
		listRic = if listSenzaTab == l then xs
					else listSenzaTab
		numTab = if not end then counter
				else lengthTab l
		numBraceClose = if end && numTab < counter then searchStack numTab stack
						else 0
		numBraceOpen = if end && numTab > counter then 1
					else 0
		newStack = if end && numBraceOpen > 0 then numTab:stack
					else if numBraceClose > 0 then reverse $ take ((length stack) - numBraceClose) (reverse stack)
						else stack
		endTok = if numBraceClose > 0 then [(info,End)]
					else []
		info = getInfo x
		tokO = (info,OpenB)
		tokC = (info,CloseB)
		endF = isEnd x
		ric = convertToken listRic numTab endF newStack
		newTok = if isTab x then []
				else [x]
		newListClose = if ifElse xs then (genNewList numBraceClose tokC)
						else (genNewList numBraceClose tokC)
		list = (genNewList2 numBraceOpen tokO) ++ newListClose++ endTok ++ newTok ++  ric

removeDupEnd [] = []
removeDupEnd [x] = [x]
removeDupEnd ((_,End):x@((info,End):xs)) = (info,End): (removeDupEnd xs) 
removeDupEnd ((_,End):(info,Token.Else):xs) = (info,Token.Else): removeDupEnd xs
removeDupEnd ((_,End):(info,Token.Elif):xs) = (info,Token.Elif): removeDupEnd xs
removeDupEnd (x:xs) = x: removeDupEnd xs


genNewList 0 _ = []
genNewList n tok = tok : (genNewList (n-1) tok)

genNewList2 0 _ = []
genNewList2 n tok = tok : ( (0,0,"") ,End) : (genNewList2 (n-1) tok)

lengthTab :: [(Info,Token)] -> Int
lengthTab [] = 0
lengthTab ((_,Tab):xs) = 1 + lengthTab xs
lengthTab (x:xs) = 0

removeTab [] = []
removeTab ((_,Tab):xs) = xs
removeTab l@(x:xs) = l
		
isTab (_,Tab) = True
isTab _ = False

isEnd (_,End) = True
isEnd _ = False

ifElse [] = False
ifElse [(_,Token.Else)] = True
ifElse (x:(_,Token.Else):xs) = True
ifElse ((_,Token.Else):xs) = True
ifElse _ = False

searchStack n [] = error $ "INDENTATION ERROR"
searchStack n (x:xs)
	| n == x = 0
	| otherwise = 1 + searchStack n xs

getInfo(info,_) = info