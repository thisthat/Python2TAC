module Env where

import Abs

newEnvirorment oldEnv = (Env [] [] oldEnv)

startEnv = (Env [] 
	[FunDef (Id "readFloat") [] Flt [] (-1,-1,"readFloat"),
	FunDef (Id "readString") [] Str [] (-1,-1,"readString"),
	FunDef (Id "readChar") [] Char [] (-1,-1,"readChar"),
	FunDef (Id "readInt") [] Integer [] (-1,-1,"readInt"),
	FunDef (Id "printFloat") [Par (Id "x") Flt ValMode] NoneType [] (-1,-1,"printFloat"),
	FunDef (Id "printString") [Par (Id "x") Str ValMode] NoneType [] (-1,-1,"printString"),
	FunDef (Id "printChar") [Par (Id "x") Char ValMode] NoneType [] (-1,-1,"printChar"),
	FunDef (Id "printInt") [Par (Id "x") Integer ValMode] NoneType [] (-1,-1,"printInt")] 
	EmptyEnv)

--Var
--Si assume che la variabile Esista
getVarEnv i@(Id id) (Env [] _ oldEnv) = getVarEnv i oldEnv
getVarEnv i@(Id id) (Env (v@(Var _ (Id nome) _ _):vs) f e) 
	| id == nome = v
	| otherwise = getVarEnv i (Env vs f e)
getVarEnv id _ = error $ (show id ++ "------")

--Boolean
--Ci dice se una var esiste nell'envirorment
existsVarEnv (Id id) EmptyEnv = False
existsVarEnv i@(Id id) (Env [] _ oldEnv) = existsVarEnv i oldEnv
existsVarEnv i@(Id id) (Env ((Var _ (Id nome) _ _ ):vs) f e) 
	| id == nome = True
	| otherwise = existsVarEnv i (Env vs f e)

--Boolean
--Ci dice se una var esiste nell'envirorment più alto
existsVarEnvTop (Id id) EmptyEnv = False
existsVarEnvTop i@(Id id) (Env [] _ oldEnv) = False
existsVarEnvTop i@(Id id) (Env ((Var _ (Id nome) _ _ ):vs) f e) 
	| id == nome = True
	| otherwise = existsVarEnvTop i (Env vs f e)


--FunDef
--Si assume che la funzione Esista
getFunEnv i EmptyEnv = (FunDef (Id "notExist") [] NoneType [] (-1,-1,"notExist"))
getFunEnv i@(Id id) (Env _ [] oldEnv) = getFunEnv i oldEnv
getFunEnv i@(Id id) (Env v (f@(FunDef (Id nome) _ _ _ _):fs) e) 
	| id == nome = f
	| otherwise = getFunEnv i (Env v fs e)

--Boolean
--Ci dice se una funzione esiste nell'envirorment
existsFunEnv (Id id) EmptyEnv = False
existsFunEnv i@(Id id) (Env _ [] oldEnv) = existsFunEnv i oldEnv
existsFunEnv i@(Id id) (Env v (f@(FunDef (Id nome) _ _ _ _):fs) e)  
	| id == nome = True
	| otherwise = existsFunEnv i (Env v fs e)

--Env
--Si assume che la variabile non sia già presente nell'env
insertVarEnv v (Env vs f oldEnv) = (Env (v:vs) f oldEnv)

--Env
--Si assume che la funzione non sia già presente nell'env
insertFunEnv f (Env v fs oldEnv) = (Env v (f:fs) oldEnv)

--Env
insertVarSmart v@(Var t id m info) e@(Env var fs oldEnv) 
	| existsVarEnv id e = updateTypeVarEnv t id m e
	| otherwise = insertVarEnv v e

--Env 
--Modifica il tipo di una variabile
--Si assume che la var esista
updateTypeVarEnv t i@(Id id) m e@(Env vs f oldEnv) 
	| existsVarEnvTop i e = (Env listVarEdit f oldEnv) 
	| otherwise = (Env vs f envEdit)
	where
		listVarEdit = updateVarList t i m vs
		envEdit = updateTypeVarEnv t i m oldEnv

--Aggiorna un elemento sulla lista
--Si assume esista all'interno della lista
updateVarList t i m (v@(Var tipo id mode info):vs) 
	| i == id = ((Var t id m info):vs) 
	| otherwise = v:(updateVarList t i m vs)



--Env
--Si aggiungono i parametri creando variabili
createEnvParam listParam (Env vs fs oldEnv) = (Env lvar fs oldEnv)
	where
		lvar = aux vs listParam
		aux vs [] = vs
		aux vs ((Par i t m):ps) = (Var t i m (-1,-1,"")) : (aux vs ps)


--FUNZIONI TOOL
getNameFromId (Id id) = id

getFunId (FunDef i _ _ _ _) = i
getLineFun (FunDef _ _ _ _ line) = line
getTypeFun (FunDef _ _ t _ _ ) = getBaseType t
getListParam id env = aux $ getFunEnv id env
	where
		aux (FunDef _ lp _ _ _) = lp

getNumParam i@(Id id) env = length $ getListParam i env

getModeParam (Par _ _ m) = m

getVarId (Var _ i _  _) = i
getLineVar (Var _ _ _ line) = line
getTypeVar (Var t _ _ _) =  t
getModeVar (Var _ _ m _ ) = m

getTypePar (Par _ t _ ) = t


--Id
getIdR (LRExp lexp) = getIdL lexp
getIdR (DRef lexp _) = getIdL lexp
getIdR (Assg lexp _ _) = getIdL lexp
getIdR (RE rexp) = getIdR rexp
getIdR (MathExp _ r1 r2 _ ) = if (getIdR r1) == (Id "") then getIdR r2
								else (getIdR r1)
getIdR _ = (Id "")
getIdL (LID id) = id
getIdL (LPunt rexp ) = getIdR rexp
getIdL (LVett lexp _ ) = getIdL lexp

--Type
--Ritorna il tipo base
getBaseType (Arr _ t) = getBaseType t
getBaseType (Ptr t) = getBaseType t
getBaseType t = t

isArray (Arr _ _) = True
isArray _ = False

isPtr (Ptr _) = True
isPtr _ = False

getArrFirstType (Arr _ t) = t
