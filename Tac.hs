module Tac where
import Abs
import TypeChecker 
import Env

data TACProgram = Program [Instruction]

data Function = Function String [Instruction]
	deriving (Eq)

data Value = 
			Constant String 			|
            Temporary Int          		|
            Variable String 			|
            Array Tac.Value Tac.Value  	|
            Dereference Tac.Value
  deriving (Eq,Show)

data Label = Label String Int
  deriving (Eq)

data Instruction = BinaryOperator Abs.BinOp Tac.Value Tac.Value Tac.Value	|
                   UnaryOperator Abs.UnOp Tac.Value Tac.Value         	  	|
                   Assignment Tac.Value Tac.Value                   		|
                   Goto Label                               				|
                   GotoIf Tac.Value Label                       			|
                   GotoIfFalse Tac.Value Label                  			|
                   OnExceptionGoto Label                    				| -- Solo inizio try
                   Param Tac.Value                              			|
                   FCall String Int Tac.Value                				|
                   FReturn Tac.Value    	                			   	|
                   PReturn             		                			  	|
                   LabelMarker Label 										|
                   DefFunction Function
  deriving (Eq)

data NextIfPos =  Else | EndIf
 deriving (Eq)

ident = showString "\t"

prettyPrintTac p = stamp 1 p $ "\n"

instance PrettyPrint TACProgram where
	stamp n (Tac.Program lstm) =  stampL n lstm

instance PrettyPrint Function where
	stamp n (Function name lstm) = showString "\n" . (showTab n) . showString "function: " . showString name . showString "\n" . stampL (n+1) lstm .
									showString "\n"

instance PrettyPrint Instruction where
	stamp n inst = case inst of
		BinaryOperator op result l r -> showTab n . stamp 0 result . showString " = " . stamp 0 l . stamp 0 op . stamp 0 r . showString "\n"
		UnaryOperator op result value -> showTab n . stamp 0 result . showString " = " . stamp 0 op . stamp 0 value . showString "\n"
		Assignment result value -> showTab n . stamp 0 result . showString " = " . stamp 0 value . showString "\n"
		Goto label ->  showTab n . showString "goto " . stamp 0 label . showString "\n"
		GotoIf value label -> showTab n . showString "if " . stamp 0 value . showString " goto " . stamp 0 label . showString "\n"
		GotoIfFalse value label -> showTab n . showString "if not " . stamp 0 value . showString " goto " . stamp 0 label . showString "\n"
		OnExceptionGoto label -> showTab n . showString "onexceptiongoto " . stamp 0 label . showString "\n"
		Param value -> showTab n . showString "param " . stamp 0 value . showString "\n"
		Tac.FCall name ariety value -> showTab n . stamp 0 value . showString " = call " . showString name . showString "/" . shows ariety . showString "\n"
		PReturn -> showTab n . showString "retp" . showString "\n"
		FReturn value -> showTab n . showString "retf " . stamp 0 value . showString "\n"
		LabelMarker label -> showTab (n-1) . stamp 0 label . showString ":\n"
		DefFunction f -> stamp n f

instance PrettyPrint Tac.Value where
	stamp n v = case v of 
		Constant s -> showString s		
		Temporary s	->  showString ('t':(show s))  
		Variable s -> showString s
		Dereference s ->  showString ('&':(show s))
		Array value1 value2 -> stamp 0 value1 . showString "[" . stamp 0 value2 . showString "]" 

instance PrettyPrint Label where
   stamp n (Label name c) = showString name . showString "_" .  (shows c)

 -- Liste "funzional level" per countinous passing style all'interno di tuple
type FList a = [a] -> [a]

sizeof :: Type -> Int
sizeof NoneType = 4
sizeof Integer = 4
sizeof Char = 1 
sizeof Flt = 8 
sizeof Str = 4
sizeof Bool = 1 
sizeof (Ptr _) = 4
sizeof (Arr n t)
	| n == 0 = 4
	| otherwise =  n * sizeof t
sizeof (Exception _) = 4 

createTac :: Abs.Program -> TACProgram
createTac (Abs.Program _ lStm) = (Tac.Program (lInst $ []))
	where
		 lInst = (createTacStmList lStm 0 0 (Label "ERROR" 0,Label "ERROR" 0) startEnv )

-- [Stm] -> Lista Stm
-- Int -> Counter Register
-- Int -> Counter Label
-- Label -> Jump continue/break 
createTacStmList :: [Stm] -> Int -> Int -> (Label,Label) -> Env -> FList Instruction
createTacStmList [] _ _ _ _ = id
createTacStmList (s:xs) c l labelCycle env =  stm .  (createTacStmList xs counter label labelCycle e)
	where 
		(counter,label,stm,e) = createTacStm s c l labelCycle env


createTacStmListBlock :: [Stm] -> Int -> Int -> (Label,Label) -> Env -> (Int,Int,FList Instruction,Env)
createTacStmListBlock [] c l _ e = (c,l,id,e)
createTacStmListBlock (s:ss) c l labelCycle env = (c'',l'', inst' . inst'', e'')
	where
		(c',l',inst',e') = createTacStm s c l labelCycle env
		(c'',l'',inst'',e'') = createTacStmListBlock ss c' l' labelCycle e'

createTacStm (SFor lexp@(LID id) rexp lstm _ ) counter label jumpExitCycle e = (cCheck,l'',inst,envFor)
	where
		info = (0,0,"")
		typeRexp = getTypeR rexp e
		typeI = (getArrFirstType typeRexp)
		env = if (existsVarEnv id e) && (isArray typeRexp) then updateTypeVarEnv typeI id ConstMode e
				else if (isArray typeRexp) then insertVarEnv (Var typeI id ConstMode info) e
					else e
		(cLexp,lLexp,addrLexp,instLexp, envL) = createTacLexp (Left lexp) counter label env typeRexp
		(cRexp,lRexp,addrRexp,instRexp, envR) = createTacRexp rexp cLexp lLexp envL
		cCheck = cRexp + 1
		lFor = lRexp + 1
		max = (getDimArray rexp env)
		labelInitFor = Label "FOR_INIT" lFor
		labelGuard = Label "FOR_GUARD" lFor
		labelExit = Label "FOR_EXIT" lFor
		varBase = (Variable "_base")
		varIndex = (Variable "_index")
		varMax =  (Variable "_max")
		varIter = (Variable "_iter")
		assegnBase = (Assignment varBase addrRexp :)
		initIndex = (Assignment varIndex (Constant "0"):)
		initMax = (Assignment varMax (Constant (show max)):)
		varCheck = (Temporary cCheck)
		varGuard = (BinaryOperator Abs.GTE varCheck varIndex varMax:)
		jumpExit = (GotoIf varCheck labelExit:)
		valoreArray = (Array varBase varIndex) 
		assVal = (Assignment addrLexp valoreArray :)
		(cFor,l'',instFor,envFor) = createTacStmListBlock lstm cCheck lFor (labelGuard,labelExit) envR
		incremento = (BinaryOperator Abs.Sum varIndex varIndex (Constant "1"):)
		jumpGuard = (Goto labelGuard:)
		inst = (LabelMarker labelInitFor:) . instLexp . instRexp . assegnBase . initIndex . initMax . 
				(LabelMarker labelGuard:) . varGuard . jumpExit . assVal . incremento . instFor . jumpGuard .
				(LabelMarker labelExit:)

createTacStm (SForElse lexp@(LID id) rexp lstm lstm2 _ ) counter label jumpExitCycle e = (cElse,lElse,inst,envForElse)
	where
		info = (0,0,"")
		typeRexp = getTypeR rexp e
		typeI = (getArrFirstType typeRexp)
		env = if (existsVarEnv id e) && (isArray typeRexp) then updateTypeVarEnv typeI id ConstMode e
				else if (isArray typeRexp) then insertVarEnv (Var typeI id ConstMode info) e
					else e
		(cLexp,lLexp,addrLexp,instLexp, envL) = createTacLexp (Left lexp) counter label env typeRexp
		(cRexp,lRexp,addrRexp,instRexp, envR) = createTacRexp rexp cLexp lLexp envL
		cCheck = cRexp + 1
		lFor = lRexp + 1
		max = (getDimArray rexp env)
		labelInitFor = Label "FOR_INIT" lFor
		labelGuard = Label "FOR_GUARD" lFor
		labelElse = Label "FOR_ELSE" lFor
		labelExit = Label "FOR_EXIT" lFor
		varBase = (Variable "_base")
		varIndex = (Variable "_index")
		varMax =  (Variable "_max")
		varIter = (Variable "_iter")
		assegnBase = (Assignment varBase addrRexp :)
		initIndex = (Assignment varIndex (Constant "0"):)
		initMax = (Assignment varMax (Constant (show max)):)
		varCheck = (Temporary cCheck)
		varGuard = (BinaryOperator Abs.GTE varCheck varIndex varMax:)
		jumpExit = (GotoIf varCheck labelElse:)
		valoreArray = (Array varBase varIndex) 
		assVal = (Assignment addrLexp valoreArray :)
		(cFor,l',instFor,envFor) = createTacStmListBlock lstm cCheck lFor (labelGuard,labelExit) envR
		incremento = (BinaryOperator Abs.Sum varIndex varIndex (Constant "1"):)
		jumpGuard = (Goto labelGuard:)
		(cElse,lElse,instForElse,envForElse) = createTacStmListBlock lstm2 cFor l' (labelGuard,labelExit) envR
		inst = (LabelMarker labelInitFor:) . instLexp . instRexp . assegnBase . initIndex . initMax . 
				(LabelMarker labelGuard:) . varGuard . jumpExit . assVal . incremento . instFor . jumpGuard . 
				(LabelMarker labelElse:) . instForElse . (LabelMarker labelExit:)

createTacStm (SWhile rexp lstm _) counter label jumpExitCycle e = (cc,ll,inst, envWhile)
	where
		info = (0,0,"")
		(err,envR) = checkRexp rexp e info
		lWhile = label + 1
		labelStm = Label "WHILE_STM" lWhile
		jumpGuard = (Goto labelGuard :)
		(cS,lS,instrS,envWhile) = createTacStmListBlock lstm counter lWhile (labelGuard,labelBreak) envR
		labelGuard = Label "WHILE_GUARD" lWhile
		(cc,ll,addr,instGuard,envG) = createTacRexp rexp cS lS envR
		jumpInit = (GotoIf addr labelStm:)	
		labelBreak = Label "WHILE_END" lWhile
		inst = jumpGuard . (LabelMarker labelStm:) . instrS . 
				(LabelMarker labelGuard :). instGuard . jumpInit . (LabelMarker labelBreak:) 

createTacStm (SWhileElse rexp lstm lstm2 _) counter label jumpExitCycle e = (cc,lGuard,inst,envWhileElse)
	where
		info = (0,0,"")
		(err,envR) = checkRexp rexp e info
		labelStm = Label "WHILE_STM" lGuard
		jumpGuard = (Goto labelGuard :)
		(cS,lS,instrS,envWhile) = createTacStmListBlock lstm counter label (labelGuard,labelBreak) envR
		lGuard = lS + 1
		labelGuard = Label "WHILE_GUARD" lGuard
		labelElse = Label "WHILE_ELSE"	lGuard
		labelBreak = Label "WHILE_END" lGuard
		(cc,ll,addr,instGuard,envG) = createTacRexp rexp cS lGuard envR
		jumpInit = (GotoIf addr labelStm :)	
		(cE,lE,instrE,envWhileElse) = createTacStmListBlock lstm2 cc ll (labelGuard,labelBreak) envR
		inst = jumpGuard . (LabelMarker labelStm:) . instrS .(LabelMarker labelGuard :) .
		 		instGuard .  jumpInit . (LabelMarker labelElse :) . 
				instrE . (LabelMarker labelBreak :) 


createTacStm (SIf rexp lstm _) counter label jumpExitCycle e = (cE,lIf,inst,envIf)
	where
		info = (0,0,"")
		(errRexp,envR) = checkRexp rexp e info
		(cRexp,lRexp,addrRexp, instRexp, envG) = createTacRexp rexp counter label envR
		ifInst = (GotoIfFalse addrRexp labelEnd :)
		(cE,lE,instrE,envIf) = createTacStmListBlock lstm cRexp lRexp jumpExitCycle envG
		lIf = lE + 1
		labelEnd = Label "IF_END" lIf
		inst = instRexp . ifInst . instrE . (LabelMarker labelEnd:)

createTacStm (SIfElse rexp stm stm2 _) counter label jumpExitCycle e = (cElse,lElse,inst,envIfElse)
	where
		info = (0,0,"")
		(errRexp,envR) = checkRexp rexp e info
		(cRexp,lRexp,addrRexp, instRexp,envG) = createTacRexp rexp counter label envR
		ifInst = (GotoIfFalse addrRexp labelElse :)
		(cIf,lIf,instrIf,envIf) = createTacStmListBlock stm cRexp lRexp jumpExitCycle envG
		gotoEnd = (Goto labelEnd :)
		labelIF = lIf + 1
		labelElse = Label "IF_ELSE" lIf
		(cElse,lElse,instrElse,envIfElse) = createTacStmListBlock stm2 cRexp labelIF jumpExitCycle envIf
		labelEnd = Label "IF_END" lIf
		inst = instRexp . ifInst . instrIf . gotoEnd .  (LabelMarker labelElse:) . 
			instrElse . (LabelMarker labelEnd:)

createTacStm (STry vex@(Var (Exception exName) _ _ _) lstTry lstmExc _) counter label jumpExitCycle e = (cEx,lEx,inst, e)
	where
		envE = insertVarEnv vex (newEnvirorment e)
		labTry = label + 1
		labelInit = Label "TRY" labTry
		labelEx = Label ("EXCEPTION_" ++ exName) labTry
		gotoCatch = (OnExceptionGoto labelEx :)
		(cTry,lTry,instrTry,envTry) = createTacStmListBlock lstTry counter label jumpExitCycle envE
		gotoEnd = (Goto labelEnd :)
		(cEx,lEx,instrEx,envC) = createTacStmListBlock lstmExc cTry lTry jumpExitCycle envE
		labelEnd = Label "TRY_END" lTry
		inst = (LabelMarker labelInit :) . gotoCatch . instrTry . gotoEnd . (LabelMarker labelEx:) 
				. instrEx . (LabelMarker labelEnd:)


createTacStm (SRExp rexp) counter label jumpExitCycle e = (cR,lR,inst,env)
	where
		(cR,lR,aR,inst,env) = createTacRexp rexp counter label e

createTacStm (SRetNoneType _) counter label jumpExitCycle e = (counter,label, (PReturn:) ,e )
createTacStm (SRet rexp _) counter label jumpExitCycle e = (counter,label, inst ,e)
	where
		(cR,lR,aR,iR,env) = createTacRexp rexp counter label e
		inst = iR . (FReturn aR :)

createTacStm (SContinue _) counter label (jumpExitCon,jumpExitBre) e = (counter, label, (Goto jumpExitCon:), e)
createTacStm (SBreak _) counter label (jumpExitCon,jumpExitBre) e = (counter, label, (Goto jumpExitBre:), e)

createTacStm (SFunDef f@(FunDef (Id name) lPar t lstm _)) counter label jumpExit e = (cFun, lFun, instr, envNext)
	where
		envNext = insertFunEnv f e
		newEnv = newEnvirorment e
		envFun = createEnvParam lPar newEnv
		labelExit = Label "ERROR" 0
		(cFun,lFun,instrFun,e') = createTacStmListBlock lstm counter label (labelExit,labelExit) envFun
		instr = (DefFunction (Function name (instrFun $ []) ) :) 

createTacStm (SDefPtr i@(Id name) t info) counter label jumpExit e = (counter,label,id, env)
	where
		exists = existsVarEnv i e
		env = if exists then updateTypeVarEnv t i ValMode e
				else insertVarEnv (Var t i ValMode info) e

createTacStm _ c l _ e = (c,l,id,e)



--IN
--	Exp, n° registro, n° label
--OUT
--Ultimo registro usato
--Ultima label usata
--Address della espressione
--Costrutto espressione
createTacRexp :: RExp -> Int -> Int -> Env -> (Int,Int,Tac.Value,FList Instruction, Env)
createTacRexp (RVal (SimpleValue t str)) counter label e = (addr,label, dest, tac, e)
	where 
		addr = counter + 1
		dest = Temporary addr
		strAp = if t == Str then "\"" ++ str ++ "\""
				else str
		tac =  (Assignment dest (Constant strAp) :)

--Dal type checker sappiamo che tutti gli elm dell'array hanno lo stesso tipo
createTacRexp (RVal (ArrayValue n rexp)) counter label e = (addr,lLr, dest, tac, eR)
	where 
		(cLr,lLr,addrLr,instLr,eR) = createTacRexpList rexp counter label e
		addr = cLr + 1
		dest = Temporary addr
		destDeref = Dereference dest
		t = if rexp == [] then NoneType
			else getTypeR (head rexp) eR
		primitiva = (Constant ("array(" ++ show n ++ "," ++ show (sizeof t) ++")") )
		base = (Assignment dest primitiva :)
		assegn = createTacArray 0 dest addrLr
		tac = instLr . base . assegn


createTacRexp (LRExp left) counter label e = createTacLexp (Right left) counter label e NoneType

createTacRexp (DRef left _) counter label e = (t, l, dest ,tacL . tac, envL)
	where
		(t,l,addr,tacL,envL) = createTacLexp (Right left) counter label e NoneType
		addr' = t + 1
		dest = Temporary addr'
		tac = (Assignment dest (Dereference addr) :)

createTacRexp (Assg left right _) counter label e = (nRegR,l, addrL, tac, eR)
	where
		t = getTypeR right e
		(nRegL,label',addrL,tacL, eL) = createTacLexp (Left left) counter label e t
		(nRegR,l,addrR,tacR,eR) = createTacRexp right nRegL label' eL
		tac = tacL . tacR . (Assignment addrL addrR :)


createTacRexp (RE rexp) counter label e = createTacRexp rexp counter label e

createTacRexp rop@(MathExp op rl rr _ ) counter label e = (addr, l, dest , tac ,eR )
	where
		(nRegL,label',addrL,tacRl,eL) = createTacRexp rl counter label e
		(nRegR,l,addrR,tacRr,eR) = createTacRexp rr nRegL label' eL
		addr = (nRegR + 1)
		dest = Temporary addr
		tac = tacRl . tacRr . (BinaryOperator op dest addrL addrR :) 

--Casi speciali per Le short-cut
createTacRexp rop@(BoolExp Abs.And r1 r2 _ ) counter label e = genExpAnd r1 r2 counter label e
createTacRexp rop@(BoolExp Abs.Or  r1 r2 _ ) counter label e = genExpOr  r1 r2 counter label e

createTacRexp rop@(BoolExp op r1 r2 _ ) counter label e = (addr, l, dest ,tac1 . tac2 . tac, e2)
	where
		(nReg1,label',addr1,tac1,e1) = createTacRexp r1 counter label e
		(nReg2,l,addr2,tac2,e2) = createTacRexp r2 nReg1 label'	e1
		addr = (nReg2 + 1)
		dest = Temporary addr
		tac = (BinaryOperator op dest addr1 addr2 :)

createTacRexp rop@(RelExp op r1 r2 _ ) counter label e = (addr, l, dest , tac, eR )
	where
		(nReg1,label',addr1,tac1,eL) = createTacRexp r1 counter label e
		(nReg2,l,addr2,tac2,eR) = createTacRexp r2 nReg1 label' e
		addr = (nReg2 + 1)
		dest = Temporary addr
		tac = tac1 . tac2 . (BinaryOperator op dest addr1 addr2 :)

createTacRexp rop@(Abs.UnExp op r _ ) counter label e = (addr,l, dest, tac, eR )
	where
		(nReg1,l,addr1,tac1,eR) = createTacRexp r counter label e
		addr = (nReg1 + 1)
		dest = Temporary addr
		tac = tac1 . (UnaryOperator op dest addr1 :)

createTacRexp (Abs.FCall (Id name) param _) counter label e = (addr, l, dest , tac, eParam)
	where
		(c,l, lAddrPar,tacGenParam,eParam) = createTacParam counter label param e
		addr = (c + 1)
		dest = Temporary addr
		tac  = tacGenParam . (pars lAddrPar) . (Tac.FCall name (length lAddrPar) dest :) 
		pars [] = id
		pars (p:ps) = (Param p:).(pars ps)

createTacRexpList :: [RExp] -> Int -> Int -> Env -> (Int, Int,[Tac.Value], FList Instruction , Env)
createTacRexpList [] counter label e = (counter,label, [] , id , e)   
createTacRexpList (r:rs) counter label e = (cL,lL, dest:destR , instR . instRL, envList)
	where
		(cR,lR,dest, instR, e') = createTacRexp r counter label e
		(cL,lL,destR,instRL, envList) = createTacRexpList rs cR lR e'


createTacLexp (Left (LID ident@(Id i))) counter label env t = (counter,label, Variable i, id, e)
	where
		line = (0,0,"")
		exists = (existsVarEnv ident env)
		mode = getModeVar(getVarEnv ident env)
		e = if exists then updateTypeVarEnv t ident mode env
			else insertVarEnv (Var t ident ValMode line) env


createTacLexp (Right (LID (Id i))) counter label e t = (counter,label, Variable i, id, e)
createTacLexp (Left l@(LPunt r )) counter label e t = (counter,label, Dereference (Variable (getNameFromId idL)), id, e)
	where
		idL = getIdL l

createTacLexp (Right l@(LPunt r )) counter label e t = createTacLexp (Left l) counter label e t 
createTacLexp (Right lexp@(LVett l r )) counter label e t = createTacLexp (Left lexp) counter label e t
createTacLexp lexp counter label e t = (c,l,(Array (Constant id) dest),tac,env)
	where
		id = getNameFromId $ getIdL $ extractEigther lexp
		nDim = dimVectorStm $ extractEigther lexp
		(c,l,dest,tac,env) = createTacVector lexp counter label e nDim
		extractEigther (Left l) = l
		extractEigther (Right l) = l


createTacVector (Left (LVett (LID idL@(Id name)) rexp)) counter label e nDim = (t0,label,temp, tac,e)
	where
		(cReg,lReg,addr,tacR,eR) = createTacRexp rexp counter label e
		varE = getVarEnv idL eR
		dim = sizeof $ getBaseType $ getTypeVar varE
		size = (Constant (show dim))
		t0 = cReg + 1
		temp = (Temporary t0)
		loc = (BinaryOperator Abs.Mul temp size addr:)
		tac =  tacR . loc 

createTacVector (Left l@(LVett lexp rexp)) counter label e nDim = (cLex,lLex,addrL,tac,eL)
	where
		(cReg,lReg,addr,tacR,eR) = createTacRexp rexp reg label e
		(cLex,lLex,addrL,tacL,eL) = createTacVector (Left lexp) cReg lReg eR nDim
		idL = (getIdL lexp)
		varE = getVarEnv idL eR
		dimSon = sizeof $ extractType nDim (getTypeVar varE)
		reg = counter + 1
		dest = (Temporary reg)
		size = (Constant (show dimSon))
		loc = (BinaryOperator Abs.Mul dest size addr:)
		sum = (BinaryOperator Abs.Sum addrL addrL dest:)
		tac = tacR . loc . tacL . sum
		extractType 0 t = t
		extractType 1 t = t
		extractType n (Arr _ t) = extractType (n-1) t
 



createTacArray :: Int -> Tac.Value -> [Tac.Value] -> FList Instruction
createTacArray n base [] = id
createTacArray n base (x:xs) = (Tac.Assignment (Array base (Constant (show n))) x :) .
								(createTacArray (n+1) base xs)


createTacParam :: Int -> Int -> [RExp] -> Env -> (Int,Int,[Tac.Value], FList Instruction, Env)
createTacParam c l [] e = (c,l,[],id,e)
createTacParam c l (p:ps) e = (c',l',lPar, tac, envPar)
	where
		(counter,label, destRexp,tacRexp,e') = createTacRexp p c l e
		(c',l', lPar',tac', envPar ) = createTacParam counter label ps e'
		lPar = destRexp:lPar'
		tac = tacRexp . tac'

genExpAnd :: RExp -> RExp -> Int -> Int -> Env -> (Int, Int,Tac.Value, FList Instruction, Env)
genExpAnd r1 r2 counter label e = (addr,lcount,dest,inst, envAnd)
	where
		(cLeft,lLeft,addrLeft,instrLeft, e1) = createTacRexp r1 counter label e
		(cRight,lRight,addrRight,instrRight, envAnd) = createTacRexp r2 cLeft lLeft e1
		addr = cRight + 1
		lcount = lRight + 1 
		dest = Temporary addr
		labelOut = Label "L_EXIT_AND" lcount
		inst = instrLeft . (GotoIfFalse addrLeft labelOut :) . instrRight . (LabelMarker labelOut:) 

genExpOr :: RExp -> RExp -> Int -> Int -> Env -> (Int, Int,Tac.Value, FList Instruction, Env)
genExpOr r1 r2 counter label e = (addr,lcount,dest,inst,envOr)
	where
		(cLeft,lLeft,addrLeft,instrLeft, e1) = createTacRexp r1 counter label e
		(cRight,lRight,addrRight,instrRight, envOr) = createTacRexp r2 cLeft lLeft e1
		addr = cRight + 1
		lcount = lRight + 1 
		dest = Temporary addr
		labelOut = Label "L_EXIT_OR" lcount
		inst = instrLeft . (GotoIf addrLeft labelOut :) . instrRight . (LabelMarker labelOut:) 
	

getDimArray (LRExp (LID id)) e = getDimFromVar (getVarEnv id e)
getDimArray (RVal (ArrayValue dim _ )) e = dim
--Per verifica di errori
getDimArray x y = error $ show (x,y)

getDimFromVar (Var (Arr dim _) _ _ _) = dim
