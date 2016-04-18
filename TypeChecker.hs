module TypeChecker where
import Abs
import Env

checkProgram (Program lImp lStm) = listError
	--(error $ (show e)) ++ listError
	where
		errImp = checkImport lImp
		(errLStm,e) = checkListStm lStm startEnv ("Main",NoneType) False
		listError = errImp ++ errLStm

checkImport lImp 
	| checkDuplicateList lImp = ["[WARNING] Duplicate Import"]
	| otherwise = []

-----------------------
---		STMS		---
-----------------------

--Flag per sapere se siamo ad un ciclo
checkListStm [] e _ flag = ([],e)

-- Controllo Sul codice dopo return 
checkListStm ((SRetNoneType info):s:ss) e (nome,tipo) flag = (err ++ ["[WARNING] Unused code after return @" ++ TypeChecker.getLine info],e)
	where
		err = if (tipo /= NoneType) then ["@" ++ TypeChecker.getLine info ++ ": Error return type (none) expected type (" ++ show tipo ++ ") in function (" ++ nome ++")"]
					else []

checkListStm ((SRet rexp info):s:ss) e (nome,tipo) flag = (err++["[WARNING] Unused code after return @" ++ TypeChecker.getLine info],e)
	where
		typeR = getTypeR rexp e 
		err = if typeR == tipo then []
				else ["@" ++ TypeChecker.getLine info ++ ": Error return type ("++ show typeR ++ ") expected type (" ++ show tipo ++ ") in function (" ++ nome ++")"]

--Tutti gli altri controlli
checkListStm (s:ss) e ftype flag = (errList ++ errList2, env')
	where
		(errList,env) = (checkStm s ftype flag e)
		(errList2,env') = (checkListStm ss env ftype flag)

--Il Flag serve per sapere se siamo all'interno di un ciclo
checkStm (SDefPtr i@(Id name) t info) ftype flag e = ([],env)
	where
		exists = existsVarEnv i e
		env = if exists then updateTypeVarEnv t i ValMode e
				else insertVarEnv (Var t i ValMode info) e

checkStm (SFor (LID id) rexp lstm info) ftype flag e = (errList,env'')
	where
		(errRexp,envR) = checkRexp rexp e info
		typeRexp = getTypeR rexp e
		typeI = (getArrFirstType typeRexp)
		env = if (existsVarEnv id e) && (isArray typeRexp) then updateTypeVarEnv typeI id ConstMode e
				else if (isArray typeRexp) then insertVarEnv (Var typeI id ConstMode info) e
					else e
		errConst = if (existsVarEnv id e) && (getModeVar (getVarEnv id e)) == ConstMode then ["@" ++ (TypeChecker.getLine info) ++ ": Can not use a constant variable in for"]
					else []
		errType = if (isArray typeRexp) then []
					else ["@" ++ (TypeChecker.getLine info) ++ ": Error in \"for\" expression\n\t Type found (" ++ show typeRexp ++ ") \n\t Expected type (Array)"]
		(errLst,env') = checkListStm lstm env ftype True
		oldMode = if (existsVarEnv id e) then getModeVar (getVarEnv id e)
					else ValMode
		env'' = updateTypeVarEnv typeI id oldMode env
		errList = errConst ++ errType ++ errRexp ++ errLst 

checkStm (SForElse (LID id) rexp lstm lstm2 info) ftype flag e = (errList,env'')
	where
		(errRexp,envR) = checkRexp rexp e info
		typeRexp = getTypeR rexp e
		typeI = (getArrFirstType typeRexp)
		env = if (existsVarEnv id e) && (isArray typeRexp) then updateTypeVarEnv typeI id ConstMode e
				else if (isArray typeRexp) then insertVarEnv (Var typeI id ConstMode info) e
					else e
		errConst = if (existsVarEnv id e) && (getModeVar (getVarEnv id e)) == ConstMode then ["@" ++ (TypeChecker.getLine info) ++ ": Can not use a constant variable in for"]
					else []
		errType = if (isArray typeRexp) then []
					else ["@" ++ (TypeChecker.getLine info) ++ ": Error in \"for\" expression\n\t Type found (" ++ show typeRexp ++ ") \n\t Expected type (Array)"]
		(errLst,env') = checkListStm lstm env ftype True
		(errLst2,env'') = checkListStm lstm2 env' ftype False
		errList = errConst ++ errType ++ errRexp ++ errLst ++ errLst2

checkStm (SWhile rexp lstm info) ftype flag e = (errList, env)
	where
		(errRexp,envR) = checkRexp rexp e info
		typeRexp = getTypeR rexp e
		errType = if any (==typeRexp) [Bool,Integer,Flt] then []
					else ["@" ++ (TypeChecker.getLine info) ++ ": Error in \"while\" guard\n\t Type found (" ++ show typeRexp ++ ") \n\t Expected type (Bool)"]
		(errLstm,env) = checkListStm lstm envR ftype True
		errList = errType ++ errLstm

checkStm (SWhileElse rexp lstm lstm2 info) ftype flag e = (errList, env')
	where
		(errRexp,envR) = checkRexp rexp e info
		typeRexp = getTypeR rexp e
		errType = if any (==typeRexp) [Bool,Integer,Flt] then []
					else ["@" ++ (TypeChecker.getLine info) ++ ": Error in \"while\" guard\n\t Type found (" ++ show typeRexp ++ ") \n\t Expected type (Bool)"]
		(errLstm,env) = checkListStm lstm envR ftype True
		(errLstm2,env') = checkListStm lstm2 env ftype False
		errList = errType ++ errLstm ++ errLstm2

checkStm (SIf rexp lstm info) ftype flag e = (errList, env)
	where
		(errRexp,envR) = checkRexp rexp e info
		typeRexp = getTypeR rexp e
		errType = if any (==typeRexp) [Bool,Integer,Flt] then []
					else ["@" ++ (TypeChecker.getLine info) ++ ": Error in \"if\" guard\n\t Type found (" ++ show typeRexp ++ ") \n\t Expected type (Bool)"]
		(errLstm,env) = checkListStm lstm envR ftype flag
		errList = errType ++ errLstm

checkStm (SIfElse rexp lstm lstm2 info) ftype flag e = (errList, env')
	where
		(errRexp,envR) = checkRexp rexp e info
		typeRexp = getTypeR rexp e
		errType = if any (==typeRexp) [Bool,Integer,Flt] then []
					else ["@" ++ (TypeChecker.getLine info) ++ ": Error in \"if\" guard\n\t Type found (" ++ show typeRexp ++ ") \n\t Expected type (Bool)"]
		(errLstm,env) = checkListStm lstm envR ftype flag
		(errLstm2,env') = checkListStm lstm2 env ftype flag
		errList = errType ++ errLstm ++ errLstm2

checkStm (STry vex lstm lstm2 info) ftype flag e = (errList, e)
	where
		(errTry,envT) = checkListStm lstm e ftype flag
		envE = insertVarEnv vex (newEnvirorment e)
		(errEx,envEx) = checkListStm lstm2 envE ftype flag
		errList = errTry ++ errEx

checkStm (SContinue info) ftype flag e = if flag then ([], e)
							else (["@" ++ TypeChecker.getLine info ++ ": found \"continue\" out of loop statement"],e)

checkStm (SBreak info) ftype flag e = if flag then ([], e)
							else (["@" ++ TypeChecker.getLine info ++ ": found \"break\" out of loop statement"],e)

checkStm (SRExp rexp) ftype flag e = checkRexp rexp e (0,0,"")

checkStm (SFunDef f@(FunDef (Id name) param tipo lstm info)) ftype flag e = (errList,envNext)
	where
		newEnv = newEnvirorment e
		envNext = insertFunEnv f newEnv
		envFun = createEnvParam param envNext
		(errStm,env) = checkListStm lstm envFun (name,tipo) False
		errDup = if checkDuplicateList param then ["@" ++ TypeChecker.getLine info ++ ": multiple definition of parameter in function \"" ++ name ++"\""]
					else []
		isRet = checkRet lstm
		errRet = case isRet of
								0 -> ["@" ++ TypeChecker.getLine info ++ ": Error missing return in (" ++ name ++ ")"]
								1 -> []
								2 -> ["@" ++ TypeChecker.getLine info ++ ": Error found return in \"if\" statement but not outside in (" ++ name ++ ")"]
								3 -> ["@" ++ TypeChecker.getLine info ++ ": Error found return in \"if\" statement but not inside \"else\" in (" ++ name ++ ")"]
								4 -> ["@" ++ TypeChecker.getLine info ++ ": Error found return in \"else\" statement but not inside \"if\" in (" ++ name ++ ")"]
		errList = errDup ++ errRet ++ errStm

checkStm (SRetNoneType info) (nome,tipo) flag e = (errTipo,e)
	where
		errTipo = if (tipo /= NoneType) then ["@" ++ TypeChecker.getLine info ++ ": Error return type (none) expected type (" ++ show tipo ++ ") in function (" ++ nome ++")"]
					else []

checkStm (SRet rexp info) (nome,tipo) flag e = (err++errRexp,envR)
	where
		(errRexp,envR) = checkRexp rexp e info
		typeR = getTypeR rexp e 
		err = if typeR == tipo then []
				else ["@" ++ TypeChecker.getLine info ++ ": Error return type ("++ show typeR ++ ") expected type (" ++ show tipo ++ ") in function (" ++ nome ++")"]

-----------------------
---		Return		---
-----------------------
existsRet [] = False
existsRet ((SRetNoneType _):ss) = True
existsRet ((SRet _ _):ss) = True
existsRet (s:ss) = existsRet ss

--Int
-- 0 -> Non esiste return
-- 1 -> Tutto Ok
-- 2 -> Esiste return nell'if ma non fuori
-- 3 -> Esiste return nell'if ma non nell'else
-- 4 -> Esiste return nell'else ma non nell'if
checkRet [] = 0
checkRet ((SIf _ lstmif _):ss)
	| existsRet lstmif && not (existsRet ss) = 2
	| not(existsRet lstmif) && (existsRet ss) = 1
	| otherwise = checkRet ss
checkRet ((SIfElse _ ls1 ls2 _):ss) 
	| existsRet ls1 && not (existsRet ls2) && not (existsRet ss) = 3
	| not(existsRet ls1) && (existsRet ls2) && not (existsRet ss) = 4
	| otherwise = checkRet ss
checkRet ((SRetNoneType _):ss) = 1
checkRet ((SRet _ _):ss) = 1
checkRet (s:ss) = checkRet ss

-----------------------
---	REXP FUNCTION	---
-----------------------

checkRexp (RVal (ArrayValue dim lrexp)) e line = (errList,e)
	where
		errListRexp = checkRexpList lrexp e
		errSameType = if lrexp /= [] && any (/= (head convertedList)) convertedList then ["@" ++ (TypeChecker.getLine line) ++ ": Error incompatibility type in Array"]
						else []
		errList = errListRexp ++ errSameType
		convertedList = convertRexp lrexp e
		convertRexp [] e = []
		convertRexp (x:xs) e = (getTypeR x e) : convertRexp xs e
		checkRexpList [] e = []
		checkRexpList (r:rs) e = errL ++ (checkRexpList rs env)
			where
				(errL,env) = checkRexp r e line

checkRexp (RVal _) e line = ([],e) 
checkRexp (DRef lexp info) e line = (err,e')
	where 
		t = getTypeL lexp e
		(errL,e') = checkLexp (Right lexp) t e info
		errId = if existsIdL lexp then []
				else ["@" ++ TypeChecker.getLine info ++ ": Error missing identifier"]
		err = errId ++ errL

checkRexp (Assg lexp rexp info) e line = (err,eR)
	where
		
		(errL,eL) = checkLexp (Left lexp) rType e info
		rType = getTypeR rexp eL
		(errR,eR) = checkRexp rexp eL info
		typeL = getTypeL lexp e
		dim = if (isArray typeL) then (dimVectorStm lexp)
				else 0
		dimR = if (isArray rType) then dimVectorDeclared rType
				else 0
		dimEnv = dimVectorDeclared typeL
		exists = if (existsIdL lexp) && existsVarEnv (getIdL lexp) e then True
					else False
		errType = if exists && (dim+dimR) /= dimEnv then ["@" ++ TypeChecker.getLine info ++ ": Error incorrect dimension type"]
					else []
		
		err = errR ++ errL ++ errType

checkRexp (LRExp lexp) env line = checkLexp (Right lexp) NoneType env line

checkRexp (RE rexp) env line = checkRexp rexp env line

checkRexp (MathExp op rexp1 rexp2 info) env line = (errList, env2)
	where
		(errR1, env1) = checkRexp rexp1 env info
		(errR2, env2) = checkRexp rexp2 env1 info
		t1 = getTypeR rexp1 env
		t2 = getTypeR rexp2 env
		tB1 = getBaseType t1
		tB2 = getBaseType t2
		dim1 = dimensionVectorRexp rexp1
		dim2 = dimensionVectorRexp rexp2
		idR1 = getIdR rexp1
		idR2 = getIdR rexp2
		var1 = getVarEnv idR1 env1
		var2 = getVarEnv idR2 env2
		errArray = if isArray t1 && ((dimVectorDeclared (getTypeVar var1)) - dim1) /= 0 then ["@"++ TypeChecker.getLine info ++ " Error: naked array expression"]
					else if isArray t2 && ((dimVectorDeclared (getTypeVar var2)) - dim2) /= 0 then ["@"++ TypeChecker.getLine info ++ " Error: naked array expression"] 
						else []
		errCompa = if (compatibilityModeMath op tB1 tB2) then []
					else ["@" ++ TypeChecker.getLine info ++ ": Error incompatibility type between (" ++ show tB1 ++ ") and (" ++ show tB2 ++ ") with (" ++ show op ++ ")"]
		errList = errR1 ++ errR2 ++ errArray ++ errCompa 
		dimensionVectorRexp (LRExp lexp) = dimensionVectorLexp lexp
		dimensionVectorRexp l = 0
		dimensionVectorLexp (LVett lexp _) = 1 + (dimensionVectorLexp lexp)
		dimensionVectorLexp _ = 0

checkRexp (BoolExp op rexp1 rexp2 info) env line = (errList, env2)
	where
		(errR1, env1) = checkRexp rexp1 env info
		(errR2, env2) = checkRexp rexp2 env1 info
		t1 = getTypeR rexp1 env
		t2 = getTypeR rexp2 env
		errType = if (t1 == Bool) && (t2 == Bool) then []
					else  ["@" ++ TypeChecker.getLine info ++ ": Error expected type bool in (" ++ show op ++ ")"]
		errList = errType ++ errR1 ++ errR2

checkRexp (RelExp op rexp1 rexp2 info) env line = (errList, env2)
	where
		(errR1, env1) = checkRexp rexp1 env info
		(errR2, env2) = checkRexp rexp2 env1 info
		t1 = getTypeR rexp1 env
		t2 = getTypeR rexp2 env
		errType = if (compatibilityTypeRel t1 t2) then []
					else  ["@" ++ TypeChecker.getLine info ++ ": Error cannot ordered element"]
		errList = errType ++ errR1 ++ errR2

checkRexp (UnExp op rexp info) env line
	| op == Not = (notErr,envR)
	| otherwise = (minErr, envR)
	where
		(errR,envR) = checkRexp rexp env info
		t = (getTypeR rexp env) 
		notErr = if (t == NoneType) || (t == Bool) then []
				else ["@" ++ TypeChecker.getLine info ++ ": Error expected bool type instead of " ++ show t]
		minErr = if t == NoneType || any (==t) [Integer,Char,Flt] then []
				else ["@" ++ TypeChecker.getLine info ++ ": Error expected \"num\" type instead of " ++ show t]

checkRexp (FCall id lRexpPar info) env line = (errList,env)
	where
		errParam = checkParam lRexpPar env info
		exists = existsFunEnv id env
		errNotEx = if exists then []
					else ["@" ++ TypeChecker.getLine info ++ ": Error function (" ++ getNameFromId id ++ ") not found"]
		errNumPar = if exists && (getNumParam id env) /= length lRexpPar then ["@" ++ TypeChecker.getLine info ++ ": Error mismatch number of parameters in (" ++ getNameFromId id ++ ")"]
					else []
		errCompa = if exists then (checkCompatibilityParam (getListParam id env) lRexpPar env (length lRexpPar) id)
					else ""
		errCompa' = if errCompa == "" then []
					else [("@" ++ TypeChecker.getLine info ++ ":") ++ errCompa]
		errList = errParam ++ errNotEx ++ errNumPar ++ errCompa'

--Prima lista formali, seconda attuali
checkCompatibilityParam [] r2 env _ id = []
checkCompatibilityParam r1 [] env _ id = []
checkCompatibilityParam (r1:rs1) (r2:rs2) env j id = if not (checkValres m1 r2) then ("\n\tError use expression with parameter declared valres # " ++ show j ++ " in (" ++ getNameFromId id ++ ")") ++ (checkCompatibilityParam rs1 rs2 env (j-1) id)
												else if (compatibilityType t1 t2) && (compatibilityMode m1 m2) then (checkCompatibilityParam rs1 rs2 env (j-1) id)
												else if not (compatibilityMode m1 m2) then ("\n\tError can not bind ValRes parameter with expression") ++ (checkCompatibilityParam rs1 rs2 env (j-1) id) 
														else ("\n\tError mismatch type of parameter # " ++ show j ++ " in (" ++ getNameFromId id ++ ")") ++ (checkCompatibilityParam rs1 rs2 env (j-1) id)
	where
		t1 = (getTypePar r1)
		t2 = (getTypeR r2 env)
		m1 = getModeParam r1
		m2 = getModeRexp r2
		compatibilityMode ConstMode ValResMode = False
		compatibilityMode _ _ = True
		getModeRexp (LRExp _ ) = ValMode
		getModeRexp _ = ConstMode
		checkValres ValResMode (LRExp _) = True
		checkValres ValResMode rexp = False
		checkValres _ _ = True


checkParam [] env info = []
checkParam (r:rs) env info = (checkParam rs env info) ++ err
	where
		(err,eR) = (checkRexp r env info)

--Type
--Data una REXP ritorna il tipo
getTypeR (RVal (SimpleValue t _)) env = t
getTypeR (RVal (ArrayValue dim t)) env 
	| t /= [] = (Arr dim (getTypeR (head t) env ))
	| otherwise = (Arr dim NoneType)
getTypeR (DRef lexp _) env = (Ptr (getTypeL lexp env))
getTypeR (Assg _ rexp _) env = getTypeR rexp env
getTypeR (FCall id _ _) env = getTypeFun (getFunEnv id env)
getTypeR (LRExp lexp) env = getTypeL lexp env
getTypeR (RE rexp) env = getTypeR rexp env
getTypeR (MathExp s r1 r2 _) env = maxType (getTypeR r1 env) (getTypeR r2 env)
getTypeR (BoolExp _ _ _ _ ) env = Bool 
getTypeR (RelExp _ _ _ _ ) env = Bool
getTypeR (UnExp s rexp _ ) env
	| s == Abs.Not = Bool
	| otherwise = getTypeR rexp env


--Ritorna il tipo massimo tra i due
--Si suppone che siano compatibili
maxType Integer Flt = Flt
maxType Integer Str = Str
maxType Char Integer = Integer
maxType Char Flt = Flt
maxType Char Str = Str
maxType Flt Str = Str
maxType _ NoneType= NoneType
maxType (Ptr t1) t2 = maxType t1 t2
maxType t1 (Ptr t2) = maxType t1 t2
maxType (Arr _ t1) t2 = maxType t1 t2
maxType t1 (Arr _ t2) = maxType t1 t2
maxType t1 _ = t1

-----------------------
---	LEXP FUNCTION	---
-----------------------

--Type
--Data una LEXP ritorna il tipo
getTypeL (LID id) env 
	| existsVarEnv id env = getTypeVar $ getVarEnv id env
	| otherwise = NoneType
getTypeL (LPunt rexp) env = getTypeR rexp env
getTypeL (LVett lexp _) env = getTypeL lexp env


--Mode
--Ritorniamo la modalità della variabile
--Sopprimiamo gli errori in modo da non duplicarli
getModeLexp (LID id) env 
	| existsVarEnv id env = getModeVar $ getVarEnv id env
	| otherwise = ValMode
getModeLexp (LPunt rexp) env 
	| existsId rexp && (existsVarEnv (getIdR rexp) env) = getModeVar ( getVarEnv (getIdR rexp) env)
	| otherwise = ValMode
getModeLexp (LVett lexp _ ) env = getModeLexp lexp env

checkLexp (Left (LID id)) newType env line = (errMode,e)
	where
		exists = (existsVarEnv id env)
		mode = getModeVar(getVarEnv id env)
		errMode = if exists && (mode == ConstMode) then ["@" ++ TypeChecker.getLine line ++ ": Error unable assign value to a var defined constant (" ++ getNameFromId id ++ ")"]
					else []
		e = if exists then updateTypeVarEnv newType id mode env
			else insertVarEnv (Var newType id ValMode line) env

checkLexp (Right (LID id)) newType env line = (err,env)
	where
		err = if (existsVarEnv id env) then []
				else ["@" ++ TypeChecker.getLine line ++ ": Error identifier (" ++ getNameFromId id ++ ") not found"]

checkLexp (Left i@(LPunt rexp)) newType env line = (errList ,e)
	where
		(errRexp,e) = checkRexp rexp env line
		errId = if existsId rexp then []
				else ["@" ++ TypeChecker.getLine line ++ ": Error expected identifier after deference operator \"^\" "]
		errType = if existsPunt rexp env then []
					else ["@" ++ TypeChecker.getLine line ++ ": Error expected pointer type after dereference operator \"^\" "]
		errList = errId ++ errType ++ errRexp

checkLexp (Right i@(LPunt rexp)) newType env line = checkLexp (Left i) newType env line

checkLexp (Left i@(LVett lexp rexp)) newType env line
	| exists && (isArray (getTypeVar varEnv)) && (dimVectorDeclared tvar) == 1+(dimVectorStm lexp) = (errRexp,e)
	| not exists = ( ["@" ++ TypeChecker.getLine line ++ ": Error identifier (" ++ getNameFromId idL ++ ") not found"],e)
	| exists && not (isArray (getTypeVar varEnv))  = (["@" ++ TypeChecker.getLine line ++ ": Error expected Array type (" ++ getNameFromId idL ++ ")"]++ errRexp,e)
	| otherwise = (["@" ++ TypeChecker.getLine line ++ ": Error cannot assign Array to Array (" ++ getNameFromId idL ++ ")"]++ errRexp,e)
	where
		idL = getIdL lexp
		varEnv = getVarEnv idL env 
		tvar = getArr varEnv
		getArr (Var t _ _ _) = t
		exists = existsVarEnv idL env
		(errRexp,e) = checkRexp rexp env line

checkLexp (Right i@(LVett lexp rexp)) newType env line = checkLexp (Left i) newType env line

--Bool
--Ritorna se in una lexp è presente un identificatore
existsId (LRExp lexp) = existsIdL lexp
existsId (DRef lexp _) = existsIdL lexp
existsId (Assg lexp _ _) = existsIdL lexp
existsId (RE rexp) = existsId rexp
existsId (MathExp _ r1 r2 _ ) = existsId r1 || existsId r2
existsId _ = False
existsIdL (LID _ ) = True
existsIdL (LPunt rexp ) = existsId rexp
existsIdL (LVett lexp _ ) = existsIdL lexp

existsPunt (LRExp lexp) env = existsPuntL lexp env
existsPunt (DRef lexp _) env = True
existsPunt (Assg lexp _ _) env  = False
existsPunt (RE rexp) env = existsPunt rexp env
existsPunt (MathExp _ r1 r2 _ ) env = existsPunt r1 env || existsPunt r2 env
existsPunt _ env = False
existsPuntL (LID id ) env = isPointerTypeId id env
existsPuntL (LPunt rexp ) env = True
existsPuntL (LVett lexp _ ) env = True

isPointerTypeId id env 
	| existsVarEnv id env && isPtr (getTypeVar (getVarEnv id env)) = True
	| otherwise = False

--num
--Ritorna la dimensione di un vettore dichiarato
dimVectorDeclared (Arr _ t) = 1 + (dimVectorDeclared t)
dimVectorDeclared _ = 0
--num
--Ritorna la dimensione di un vettore stm
dimVectorStm (LVett l _) = 1 + (dimVectorStm l)
dimVectorStm _ = 0

--TOOL
--Bool
checkDuplicateList [] = False
checkDuplicateList (x:xs) = any (==x) xs || checkDuplicateList xs


-----------------------
---		TIPI		---
-----------------------

--Bool
--Compatibilità per espressioni matematiche e assegnamenti
compatibilityType Flt Bool = False
compatibilityType Flt Str = False
compatibilityType Char Str = False
compatibilityType Char Bool = False
compatibilityType Char Flt = False
compatibilityType Char Integer = False
compatibilityType Integer Bool = False
compatibilityType Integer Str = False
compatibilityType Integer Flt = False
compatibilityType Bool Char = False
compatibilityType Bool Integer = False
compatibilityType Bool Flt = False
compatibilityType Bool Str = False
compatibilityType t1 NoneType = False
compatibilityType NoneType t1 = True
compatibilityType (Ptr t1) t2 = compatibilityType t1 t2
compatibilityType (Arr _ t1) t2 = compatibilityType t1 t2
compatibilityType t1 (Ptr t2) = compatibilityType t1 t2
compatibilityType t1 (Arr _ t2) = compatibilityType t1 t2
compatibilityType _ _  = True

compatibilityModeMath op (Arr _ _) t2 = False
compatibilityModeMath op t1 (Arr _ _) = False
compatibilityModeMath op (Exception _) t2 = False
compatibilityModeMath op t1 (Exception _) = False
compatibilityModeMath op (Ptr ptr) t2 =  compatibilityModeMath op ptr t2
compatibilityModeMath op t1 (Ptr ptr) =  compatibilityModeMath op t1 ptr
compatibilityModeMath op Str Integer 
	| op == Abs.Sum = True
	| otherwise = False
compatibilityModeMath op Integer Str 
	| op == Abs.Sum = True
	| otherwise = False
compatibilityModeMath op t1 t2
	| (t1 == Bool) || (t2 == Bool) = False
	| (t1 == NoneType) || (t2 == NoneType) = False
	| otherwise = True


--Bool
--Compatibilità per espressioni booleane
compatibilityTypeRel NoneType _ = False
compatibilityTypeRel _ NoneType = False
compatibilityTypeRel Integer Str = False
compatibilityTypeRel Str Integer = False
compatibilityTypeRel Char Str = False
compatibilityTypeRel Str Char = False
compatibilityTypeRel Flt Str = False
compatibilityTypeRel Str Flt = False
compatibilityTypeRel Bool _ = False
compatibilityTypeRel _ Bool = False
compatibilityTypeRel _ _ = True

getLine (c,l,_) = show l

