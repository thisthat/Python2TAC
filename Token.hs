module Token where

-- Token 

data Token = 
-- Identificatori
	Def 		|
	Import		|

-- Spaziature
	Tab			|
	Space		|

-- Punteggiatura
	OpenR 		|
	CloseR 		|
	OpenS 		| 
	CloseS 		| 
	OpenB 		| 
	CloseB 		|
	Comma 		| 
	Semicol 	|
	Col 		|
	Dot			|

-- Operatori
	Equal		|
	Plus		|
	Minus 		| 
	Div 		| 
	Star		|
	OpDiseqL 	| 
	OpDiseqG 	|
	OpDiseqLE 	| 
	OpDiseqGE 	| 
	OpEq 		| 	
	OpNeq 		|
	And 		| 
	Or 			| 
	Not			|
	DeRef		|
	Ref 		|

-- Dichiarazioni di tipo
 	TNoneType 		| 
 	TChar 			| 
 	TBool 			| 
 	TInt 			| 
 	TStr 			| 
 	TFloat			| 
 	TPtr			|
	Ident String 		|
	Boolean String 		| 
	Integer String 		| 
	Real String 		| 
	String String 		| 
	Char String 		|
	
-- Strutture di controllo
	If 			|
	Elif 		| 
	Else 		| 
	While 		| 
	For 		| 
	In 			| 
	Try 		| 
	Except 		| 
	As			|			-- 'as' non e' una struttura di controllo, ma e' una keyword utilizzata in queste ultime
	Continue 	| 
	Break 		| 
	Return		|

-- Passaggio parametri
	Cnst		|
	Val 		| 
	Valres 		|

-- Carriage Return
	End			|

-- Indefiniti
	LexUndef 	|

-- Supporto futuro
	Indent 		| 
	Dedent

	deriving(Eq, Show)