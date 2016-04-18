{
module Parser where

--import Dati
import Lexer
import Token
import qualified Abs
--import Function
--import PrettyPrint

{-
terminal TAB is unused
terminal SPACE is unused
terminal DOT is unused
terminal INDENT is unused
terminal DEDENT is unused
-}

}

%name parseProgram
%tokentype { (Abs.Info,Token) }
%error { parseError }

%token 

DEF				{ (a, Token.Def) }
IMPORT			{ (a, Token.Import) }
-- TAB				{ (a, Token.Tab) }
-- SPACE			{ (a, Token.Space) }
END				{ (a, Token.End) }
OPENR			{ (a, Token.OpenR) }
CLOSER			{ (a, Token.CloseR) }
OPENS			{ (a, Token.OpenS) }
CLOSES			{ (a, Token.CloseS) }
OPENB			{ (a, Token.OpenB) }
CLOSEB			{ (a, Token.CloseB) }
COMMA			{ (a, Token.Comma) }
SEMICOL			{ (a, Token.Semicol) }
COL				{ (a, Token.Col) }
-- DOT				{ (a, Token.Dot) }
EQUAL			{ (a, Token.Equal) }
PLUS			{ (a, Token.Plus) }
MINUS			{ (a, Token.Minus) }
STAR			{ (a, Token.Star) }
DIV				{ (a, Token.Div) }
OPDISEQL		{ (a, Token.OpDiseqL) }
OPDISEQG		{ (a, Token.OpDiseqG) }
OPDISEQLE		{ (a, Token.OpDiseqLE) }
OPDISEQGE		{ (a, Token.OpDiseqGE) }
OPEQ			{ (a, Token.OpEq) }
OPNEQ			{ (a, Token.OpNeq) }
AND 			{ (a, Token.And) }
OR				{ (a, Token.Or) }
NOT				{ (a, Token.Not) }
DEREF			{ (a, Token.DeRef) }
REF				{ (a, Token.Ref) }
IDENT			{ (a, Token.Ident $$) }
BOOLEAN			{ (a, Token.Boolean $$) }
INTEGER			{ (a, Token.Integer $$) }
REAL			{ (a, Token.Real $$) }
STRING			{ (a, Token.String $$) }
CHAR			{ (a, Token.Char $$) }
TNODETYPE		{ (a, Token.TNoneType) }
TCHAR			{ (a, Token.TChar) }
TBOOL			{ (a, Token.TBool) }
TINT			{ (a, Token.TInt) }
TSTR			{ (a, Token.TStr) }
TFLOAT			{ (a, Token.TFloat) }
TPTR			{ (a, Token.TPtr) }
VAL 			{ (a, Token.Val) }
VALRES			{ (a, Token.Valres) }
CNST			{ (a, Token.Cnst) }
IF				{ (a, Token.If) }
ELIF			{ (a, Token.Elif) }
ELSE			{ (a, Token.Else) }
WHILE			{ (a, Token.While) }
FOR 			{ (a, Token.For) }
IN 				{ (a, Token.In) }
TRY				{ (a, Token.Try) }
EXCEPT			{ (a, Token.Except) }
AS				{ (a, Token.As) }
CONTINUE		{ (a, Token.Continue) }
BREAK			{ (a, Token.Break) }
RETURN			{ (a, Token.Return) }
-- INDENT			{ (a, Token.Indent) }
-- DEDENT			{ (a, Token.Dedent) }

-- Precedenze per disambiguare la grammatica
%nonassoc EQUAL
%left OPENR
%left OPENS
%left OPENB
%left OR
%left AND
%left OPEQ
%left OPNEQ
%left OPDISEQL
%left OPDISEQLE
%left OPDISEQG
%left OPDISEQGE
%left PLUS 
%left MINUS
%left STAR
%left DIV
%left NOT
%left DEREF
%left REF

%%

-- Definizione della grammatica

Program: ImportList Statement { Abs.Program $1 $2 }

--Gli import si posso ignorare 
ImportList	: {-empty-} { [] }
			| IMPORT IDENT COMMA ImportListNotEmpty { (Abs.Import $2) : $4}
			| IMPORT IDENT END ImportList { (Abs.Import $2) : $4 }

ImportListNotEmpty 	: IMPORT IDENT { [(Abs.Import $2)] }
					| IMPORT IDENT COMMA ImportListNotEmpty { (Abs.Import $2):$4 }

   	
Statement: ListStm { $1 } 

ListStm : {-empty-} { [] }
	| Stm SEMICOL ListStm 		{ ($1 : $3) }
    | Stm END ListStm 			{ ($1 : $3) }

NotEmptyListStm : Stm SEMICOL NotEmptyListStm 	{ ($1 : $3) }
    			| Stm END NotEmptyListStm 		{ ($1 : $3) }
    			| Stm 							{ [$1] }

Stm : FOR IDENT IN Re COL Suite					{ (Abs.SFor (Abs.LID (Abs.Id $2)) $4 $6 (getT $1)) }         
	| FOR IDENT IN Re COL Suite ELSE COL Suite	{ (Abs.SForElse (Abs.LID (Abs.Id $2)) $4 $6 $9 (getT $1)) }
	| WHILE Re COL Suite						{ (Abs.SWhile $2 $4 (getT $1)) }
	| WHILE Re COL Suite ELSE COL Suite			{ (Abs.SWhileElse $2 $4 $7 (getT $1)) }
	| IF Re COL Suite							{ (Abs.SIf $2 $4 (getT $1)) }
	| IF Re COL Suite ELSE COL Suite 			{ (Abs.SIfElse $2 $4 $7 (getT $1)) }
	| IF Re COL Suite ListElifs					{ (Abs.SIfElse $2 $4 $5 (getT $1)) }
	| TRY COL Suite EXCEPT IDENT AS IDENT COL Suite	
	{ (Abs.STry (Abs.Var (Abs.Exception $5) (Abs.Id $7) Abs.ConstMode (getT $6)) $3 $9 (getT $4)) }
	| Re  											{ (Abs.SRExp $1) }
	| RETURN     									{ (Abs.SRetNoneType (getT $1)) }
	| RETURN Re 									{ (Abs.SRet $2 (getT $1)) }
	| CONTINUE 										{ (Abs.SContinue (getT $1)) }
	| BREAK 										{ (Abs.SBreak (getT $1)) }
	| TPTR IDENT Type									{ (Abs.SDefPtr (Abs.Id $2) (Abs.Ptr $3) (getT $1))}
	| DEF IDENT OPENR ParList CLOSER Type COL Suite	
	{ (Abs.SFunDef (Abs.FunDef (Abs.Id $2) $4 $6 $8 (getT $1))) }

--Tornano una lista di STM
ListElifs 	: ELIF Re COL Suite ELSE COL Suite { [(Abs.SIfElse $2 $4 $7 (getT $1)) ]}
			| ELIF Re COL Suite ListElifs { [(Abs.SIfElse $2 $4 $5 (getT $1)) ]}

Suite  	: END OPENB END ListStm OptEnd CLOSEB 			{ $4 }
        | END OPENB END NotEmptyListStm OptEnd CLOSEB 	{ $4 }

Values 	: INTEGER	{ (Abs.SimpleValue Abs.Integer $1) }
		| REAL		{ (Abs.SimpleValue Abs.Flt $1) }
		| BOOLEAN	{ (Abs.SimpleValue Abs.Bool $1) }
		| STRING	{ (Abs.SimpleValue Abs.Str $1) }
		| CHAR		{ (Abs.SimpleValue Abs.Char $1) }
		| OPENS ListElmArr CLOSES { Abs.ArrayValue (length $2) $2 }

Type : {-empty-}	{ Abs.NoneType }
	 | TNODETYPE	{ Abs.NoneType }
	 | TCHAR		{ Abs.Char }
	 | TBOOL		{ Abs.Bool }
	 | TSTR			{ Abs.Str }
	 | TINT			{ Abs.Integer }
	 | REF Type		{ Abs.Ptr $2 }
	 | TFLOAT		{ Abs.Flt }
	 | Type OPENS CLOSES { Abs.Arr 0 $1 }

ListElmArr 	: {-empty-} 				{ [] }
			| Re					{ [$1] }
			| Re COMMA ListElmArrNotEmpy 	{ $1:$3 }

ListElmArrNotEmpy 	: Re							{ [$1] }
					| Re COMMA ListElmArrNotEmpy 	{ $1:$3 }

ParList: {-empty-}     		{ [] } 
       | ParListNE     		{ $1 }

ParListNE : Mode IDENT COL Type 			 	 { [(Abs.Par (Abs.Id $2) $4 $1)] }
		  | ParListNE COMMA Mode IDENT COL Type  { (Abs.Par (Abs.Id $4) $6 $3) : $1 }
 
          
Mode : VALRES 		{ Abs.ValResMode }
     | CNST			{ Abs.ConstMode }
     | {-empty-} 	{ Abs.ValMode }
     | VAL 			{ Abs.ValMode } 


Re  : Re PLUS Re					{ (Abs.MathExp Abs.Sum $1 $3 (getT $2)) }
	| Re MINUS Re					{ (Abs.MathExp Abs.Sub $1 $3 (getT $2)) }
	| Re STAR Re 					{ (Abs.MathExp Abs.Mul $1 $3 (getT $2)) }
	| Re DIV Re						{ (Abs.MathExp Abs.Div $1 $3 (getT $2)) }
	| Re STAR STAR Re				{ (Abs.MathExp Abs.Pow $1 $4 (getT $2)) }
	| Re OPDISEQL Re				{ (Abs.RelExp Abs.LT $1 $3 (getT $2)) }
	| Re OPDISEQLE Re				{ (Abs.RelExp Abs.LTE $1 $3 (getT $2)) }
	| Re OPDISEQG Re				{ (Abs.RelExp Abs.GT $1 $3 (getT $2)) }
	| Re OPDISEQGE Re				{ (Abs.RelExp Abs.GTE $1 $3 (getT $2)) }
	| Re OPEQ Re					{ (Abs.BoolExp Abs.Equ $1 $3 (getT $2)) }
	| Re OPNEQ Re					{ (Abs.BoolExp Abs.NEqu $1 $3 (getT $2)) }
	| Re OR Re  					{ (Abs.BoolExp Abs.Or $1 $3 (getT $2)) }
	| Re AND Re						{ (Abs.BoolExp Abs.And $1 $3 (getT $2)) }
	| OPENR Re CLOSER				{ (Abs.RE $2) }
	| MINUS Re						{ (Abs.UnExp Abs.Minus $2 (getT $1)) }
	| NOT Re						{ (Abs.UnExp Abs.Not $2 (getT $1)) }
	| Le							{ (Abs.LRExp $1) }
	| Le EQUAL Re					{ (Abs.Assg $1 $3 (getT $2)) }
	| IDENT OPENR ListParAtt CLOSER	{ (Abs.FCall (Abs.Id $1) $3 (getT $2)) }
	| DEREF Le						{ (Abs.DRef $2 (getT $1)) }
	| Values						{ (Abs.RVal $1) }

ListParAtt: {-empty-}     		{ [] } 
       	  | ListParAttNE     	{ $1 }

ListParAttNE : Re 				 	 { [$1]  }
          	 | ListParAttNE COMMA Re { $3 : $1 }

Le 	: IDENT 					{ (Abs.LID (Abs.Id $1)) }
	| REF IDENT					{ (Abs.LPunt (Abs.LRExp (Abs.LID (Abs.Id $2)))) }
	| REF OPENR Re CLOSER 		{ (Abs.LPunt $3) }
	| Le OPENS Re CLOSES		{ (Abs.LVett $1 $3) }

OptEnd : {-empty-}	{}
	   | END 		{}

--LEFT RECURSION

{ 

{-
main = do
	inStr <- getContents
	let pParsed = parseProgram (alexScanTokens inStr)
	let errList = typeCheck pParsed
	putStrLn ""
	if errList == [] then writeFile "prettyPrintedFile.txt" (prettyPrint pParsed)
	else writeError errList
	putStrLn ""
	where
		writeError [] = putStrLn "-- Fine --"
		writeError (x:xs) = (writeColor x) >> (writeError xs)
		writeColor x = if (take 6 x) == "[WARN]" then colorPutStrLn Yellow x 
			else putStrLn x
-}


getT (t,_) = t


-- error reporting
parseError (((c,l,s), Token.LexUndef ):xs) = error $ "Lexical error at : "++show(l)++","++show(c)++", near: "++s

parseError (((c,l,s), _ ):xs) = error $ "Syntax error at : "++show(l)++","++show(c)++" near: "++s
parseError x = error $ show "Missing Return character at and of file"

}


