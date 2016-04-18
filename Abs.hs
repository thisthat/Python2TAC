module Abs where

-- Token Information: Line,Column,Lexema
-- Per ogni nodo dell'albero che puo' essere usato per migliorare l'error reporting
-- e' stato aggiunto l'attributo info
type Info = (Int,Int,String)

-- Tipo di dato per salvare le dichiarazioni di variabile
-- Tipo, Identificatore, Valore Inizializzazione
-- Mode serve per convertire i parametri in variabili
data Var = Var Type Id Mode Info deriving (Eq, Show, Ord)


-- Tipi Primitivi
-- Il tipo Exception serve per il try catch
data Type = NoneType | Integer | Char | Flt | Str | Bool | Ptr Type | Arr Int Type | Exception String
	deriving (Eq, Show, Ord)

-- Identificatore
data Id = Id String  deriving (Eq, Show, Ord)

-- Valore inizializzazione Variabile
data Value = 
	  SimpleValue Type String 
	| ArrayValue Int [RExp]
	deriving (Eq, Show, Ord)
			
-- Parametri: Identificatore, Tipo, Modalita' di passaggio
data Param = Par Id Type Mode deriving (Show, Ord)

instance Eq Param where
	x == y =  getIdPar x == getIdPar  y 

getIdPar (Par i _ _) = i

-- Le 3 modalita' classiche
data Mode = ValMode | ConstMode | ValResMode 
		deriving (Eq, Show, Ord)

data Program = Program [Import] [Stm] deriving (Show,Eq,Ord)

data Import = Import String deriving (Show,Eq,Ord)

data FunDef = FunDef Id [Param] Type [Stm] Info
	deriving (Eq,Show,Ord)

-- Statement
data Stm = 
	  SFor LExp RExp [Stm] Info
	| SForElse LExp RExp [Stm] [Stm] Info
	| SWhile RExp [Stm] Info
	| SWhileElse RExp [Stm] [Stm] Info
	| SIf RExp [Stm] Info
	| SIfElse RExp [Stm] [Stm] Info
	| STry Var [Stm] [Stm] Info
	| SRExp RExp 
	| SRetNoneType Info
	| SRet RExp Info
	| SContinue Info
	| SBreak Info
	| SFunDef FunDef
	| SDefPtr Id Type Info
	deriving (Eq, Show, Ord)

-- Right Expression
data RExp = 
	  RVal Value
	| DRef LExp Info
	| Assg LExp RExp Info
	| FCall Id [RExp] Info
	| LRExp LExp
	| RE RExp
	| MathExp BinOp RExp RExp Info -- + - * / **
	| BoolExp BinOp RExp RExp Info	-- And Or 
	| RelExp BinOp RExp RExp Info 	-- < <= > >=
	| UnExp UnOp RExp Info			-- Not Minus
	deriving (Eq, Show, Ord)

-- Left Expression
data LExp = LID Id 
	| LPunt RExp 
	| LVett LExp RExp 
	deriving (Eq, Show, Ord)


data BinOp = Sum | Sub | Mul | Div | Pow |
             And | Or | 
             Equ | NEqu | LT | GT | LTE | GTE
			deriving (Eq,Ord)

data UnOp = Minus | Not
 			deriving (Eq,Ord)	

-- Definisce l'envirorment usato per il type checking
data Env = Env [Var] [FunDef] Env | EmptyEnv deriving (Eq,Show)


prettyPrintProgram p = stamp 0 p $ "\n"

showTab 0 = showString ""
showTab n = showString "\t" . showTab (n-1)

class PrettyPrint a where
  stamp :: Int -> a -> (String -> String)
  stampL :: Int -> [a] -> (String -> String)
  stampL n = (foldr (.) id) . map (stamp n)

instance PrettyPrint Program where
 	stamp n (Program lImp lStm) =  (showTab n) . (stampL n lImp) . showString "\n" . stampL n lStm

instance PrettyPrint Import where
	stamp n (Import name) = (showTab n) . showString "import " . (showString name) . showString "\n"

instance PrettyPrint Stm where
	stamp n stm = case stm of
		SFor lexp rexp lStm _ -> (showTab n) . showString "for " . (stamp 0 lexp) . showString " in " . (stamp 0 rexp) . showString ":\n" . 
									(showTab n) . showString "\n" . (stampL (n+1) lStm) . showString "\n"
		SForElse lexp rexp lstm1 lstm2 _ -> (showTab n) . showString "for " . (stamp 0 lexp) . showString " in " . 
											(stamp 0 rexp) . showString ":\n" . (stampL (n+1) lstm1) .
											showString "\n" . (showTab n) . showString "else :\n" . (stampL (n+1) lstm2) . 
											showString "\n"
		SWhile rexp lStm _ -> (showTab n) . showString "while " . (stamp n rexp) . showString ":\n" . showTab n . showString "" . 
								(stampL (n+1) lStm) . showString "\n"
		SWhileElse rexp lstm1 lstm2 _ -> (showTab n) . showString "while " . (stamp n rexp) . showString ":\n" . 
											(showTab n) . (stampL (n+1) lstm1) . showString "\n" . (showTab n) . showString "else :\n" . 
											stampL (n+1) lstm2 . showString "\n"
		SIf rexp lstm _ -> (showTab n) . showString "if " . (stamp n rexp) . showString ":\n" . showTab n  .
							stampL (n+1) lstm . showString "\n"
		SIfElse rexp lstm lstm2 _ -> (showTab n) . showString "if " . (stamp n rexp) . showString ":\n" .
										stampL (n+1) lstm . showTab n . showString "else :\n" .
										stampL (n+1) lstm2 . showTab n . showString "\n"
		STry (Var (Exception name) (Id var) _ _) lstm lstm2 _ -> showTab n . showString "try:\n" . showString "\n" . stampL (n+1) lstm .
																showTab n . showString "except " . showString name . showString " as " .
																showString var . showString ":\n"  . stampL (n+1) lstm2 .
																showString "\n"
		SRExp rexp  ->	showTab	n . stamp n rexp . showString "\n"
		SRetNoneType _ -> showTab n . showString "return\n"
		SRet rexp _ -> showTab n . showString "return " . stamp n rexp . showString "\n"
		SContinue _ -> showTab n . showString "continue " . showString "\n"
		SBreak _ -> showTab n . showString "break " . showString "\n"
		SFunDef (FunDef (Id name) lPar t lstm _) -> showTab n . showString "def " . showString name . showString "(" . stampPar n (reverse lPar) . showString ") " .
													(stamp n t) . showString ":\n" . stampL (n+1) lstm . showString "\n" 
		SDefPtr (Id id) t _ -> showTab n . showString "pointer " . showString id . showString " " . stamp n t
		where
			stampPar _ [] = id
			stampPar n [x] = stamp n x
			stampPar n (x:xs) = stamp n x . showString "," . stampPar n xs

instance PrettyPrint RExp where
	stamp n rexp = case rexp of
		RVal value -> stamp n value
	 	DRef lexp _ -> stamp n lexp
		Assg lexp rexp _ -> stamp n lexp . showString " = " . stamp n rexp
		FCall (Id name) lrexp _ -> showString name . showString "(" . (stampaRexpParam lrexp n) . showString ")"
		LRExp lexp -> stamp n lexp
		RE rexp -> showString "(" . stamp n rexp . showString ")"
		MathExp op rexp1 rexp2 _ -> stamp n rexp1 . stamp 0 op . stamp n rexp2 
	 	BoolExp op rexp1 rexp2 _ -> stamp n rexp1 . stamp 0 op . stamp n rexp2 
		RelExp op rexp1 rexp2 _  -> stamp n rexp1 . stamp 0 op . stamp n rexp2 
		UnExp op rexp _ -> stamp 0 op . stamp n rexp
		where
			stampaRexpParam :: [RExp] -> Int -> (String -> String)
			stampaRexpParam [] _ = id
			stampaRexpParam [x] n = stamp n x
			stampaRexpParam (x:xs) n = stamp n x . showString "," . stampaRexpParam xs n

instance PrettyPrint LExp where
	stamp n lexp = case lexp of
		LID (Id id) -> showString id
		LPunt rexp -> showString "^" . stamp n rexp
		LVett lexp rexp -> stamp n lexp . showString "[" . stamp n rexp . showString "]"

instance PrettyPrint Value where
	stamp n val = case val of
		SimpleValue t val -> case t of 
								Str -> showString (show val)
								_ -> showString val
		ArrayValue _ lRexp -> showString "[" . stampaRexpArray lRexp n . showString "]"
		where
			stampaRexpArray :: [RExp] -> Int -> (String -> String)
			stampaRexpArray [] _ = id
			stampaRexpArray [x] n = stamp n x
			stampaRexpArray (x:xs) n = stamp n x . showString "," . stampaRexpArray xs n

instance PrettyPrint Param where
	stamp n (Par (Id nome) t mode) = stamp n mode . showString nome . showString ":" .  stamp n t

instance PrettyPrint Mode where
	stamp n mode = case mode of
		ValMode -> showString "val " 
		ConstMode -> showString "cnst " 
		ValResMode -> showString "valres " 

instance PrettyPrint Type where
	stamp n t = case t of
		NoneType -> showString "none"
		Integer -> showString "int"
		Char -> showString "char"
		Flt -> showString "float"
		Str -> showString "string"
		Bool -> showString "bool"
		Ptr t -> showString "^" . stamp n t
		Arr _ t -> stamp n t . showString "[]"
		Exception name -> showString name

instance PrettyPrint BinOp where
	stamp n op = case op of
		Abs.Sum -> showString " + " 
		Abs.Sub -> showString " - " 
		Abs.Mul -> showString " * " 
		Abs.Div -> showString " / " 
		Abs.Pow -> showString " ** " 
		Abs.And -> showString " and " 
		Abs.Or -> showString " or " 
		Abs.Equ -> showString " == "
		Abs.NEqu -> showString " != " 
		Abs.LT -> showString " < "  
		Abs.GT -> showString " > "
		Abs.LTE -> showString " <= "  
		Abs.GTE -> showString " >= " 

instance PrettyPrint UnOp where
	stamp n op = case op of
		Abs.Minus -> showString " -"
		Abs.Not -> showString " !" 

instance Show BinOp where
	showsPrec _ op = case op of
		Abs.Sum -> showString " + " 
		Abs.Sub -> showString " - " 
		Abs.Mul -> showString " * " 
		Abs.Div -> showString " / " 
		Abs.Pow -> showString " ** " 
		Abs.And -> showString " and " 
		Abs.Or -> showString " or " 
		Abs.Equ -> showString " == "
		Abs.NEqu -> showString " != " 
		Abs.LT -> showString " < "  
		Abs.GT -> showString " > "
		Abs.LTE -> showString " <= "  
		Abs.GTE -> showString " >= " 

instance Show UnOp where
	showsPrec _ op = case op of
		Abs.Minus -> showString " -"
		Abs.Not -> showString " !" 