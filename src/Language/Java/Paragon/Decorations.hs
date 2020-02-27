{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# Language TemplateHaskell#-}
module Language.Java.Paragon.Decorations where
import Language.Java.Paragon.SourcePos
import Language.Java.Paragon.TypesTTG
import Language.Java.Paragon.SyntaxTTG
import Data.Void
import Data.Data
-- import Language.Java.Paragon.SyntaxTTG

-- | Data type for when an extension field is not used. (As in Trees That Grow)
type NoFieldExt = ()
-- | Data type for when a extension constructor is not used. (As in Trees That Grow)
type NoConExt = Void

--------------------------------------------------------------------------------
-- Parser
--------------------------------------------------------------------------------
data Pa deriving Data

type instance XSP                  Pa = SourcePos
type instance XCompilationUnit     Pa = NoFieldExt
type instance XPackageDecl         Pa = NoFieldExt
type instance XImportDecl          Pa = NoFieldExt
type instance XTypeDecl            Pa = NoFieldExt
type instance XClassDecl           Pa = NoFieldExt
type instance XClassBody           Pa = NoFieldExt
type instance XEnumBody            Pa = NoFieldExt
type instance XEnumConstant        Pa = NoFieldExt
type instance XInterfaceDecl       Pa = NoFieldExt
type instance XInterfaceBody       Pa = NoFieldExt
type instance XDecl                Pa = NoFieldExt
type instance XMemberDecl          Pa = NoFieldExt
type instance XVarDecl             Pa = NoFieldExt
type instance XVarDeclId           Pa = NoFieldExt
type instance XVarInit             Pa = NoFieldExt
-- type instance XInitExp             Pa = NoFieldExt
type instance XFormalParam         Pa = NoFieldExt
type instance XMethodBody          Pa = NoFieldExt
type instance XConstructorBody     Pa = NoFieldExt
type instance XExplConstrInv       Pa = NoFieldExt
type instance XMod                 Pa = NoFieldExt
type instance XBlock               Pa = NoFieldExt
type instance XBlockStm            Pa = NoFieldExt
type instance XStm                 Pa = NoFieldExt
type instance XCatch               Pa = NoFieldExt
type instance XSwitchBlock         Pa = NoFieldExt
type instance XSwitchLabel         Pa = NoFieldExt
type instance XForInit             Pa = NoFieldExt
type instance XExceptionSpec       Pa = NoFieldExt
type instance XExp                 Pa = NoFieldExt
type instance XLiteral             Pa = NoFieldExt
type instance XOp                  Pa = NoFieldExt
type instance XAssignOp            Pa = NoFieldExt
type instance XLhs                 Pa = NoFieldExt
type instance XArrayIndex          Pa = NoFieldExt
type instance XFieldAccess         Pa = NoFieldExt
type instance XMethodInvocation    Pa = NoFieldExt
type instance XArrayInit           Pa = NoFieldExt
type instance XReturnType          Pa = NoFieldExt
type instance XType                Pa = NoFieldExt
type instance XRefType             Pa = NoFieldExt
type instance XClassType           Pa = NoFieldExt
type instance XTypeArgument        Pa = NoFieldExt
type instance XNonWildTypeArgument Pa = NoFieldExt
type instance XWildcardBound       Pa = NoFieldExt
type instance XPrimType            Pa = NoFieldExt
type instance XTypeParam           Pa = NoFieldExt
type instance XPolicyExp           Pa = NoFieldExt
type instance XLockProperties      Pa = NoFieldExt
type instance XClause              Pa = NoFieldExt
type instance XClauseVarDecl       Pa = NoFieldExt
type instance XClauseHead          Pa = NoFieldExt
type instance XLClause             Pa = NoFieldExt
type instance XActor               Pa = NoFieldExt
type instance XActorName           Pa = NoFieldExt
type instance XAtom                Pa = NoFieldExt
type instance XLock                Pa = NoFieldExt
type instance XIdent               Pa = NoFieldExt
type instance XName                Pa = NoFieldExt

-- Pattern synonyms for parser

pattern PTypeParam sp id re = TypeParam () sp id re

pattern PCompilationUnit sp mpd id td = CompilationUnit () sp mpd id td

pattern PPackageDecl sp n = PackageDecl () sp n

pattern PSingleTypeImport sp n = SingleTypeImport () sp n
pattern PTypeImportOnDemand sp n = TypeImportOnDemand () sp n
pattern PSingleStaticImport sp n id = SingleStaticImport () sp n id
pattern PStaticImportOnDemand sp n = StaticImportOnDemand () sp n


pattern PClassTypeDecl sp cl = ClassTypeDecl () sp cl
pattern PInterfaceTypeDecl sp id = InterfaceTypeDecl () sp id

pattern PClassDecl sp mod id tp mct ct cb = ClassDecl () sp mod id tp mct ct cb
pattern PEnumDecl sp mod id ct eb = EnumDecl () sp mod id ct eb

pattern PClassBody sp d = ClassBody () sp d

pattern PEnumBody sp ec d = EnumBody () sp ec d

pattern PEnumConstant sp id arg mcb = EnumConstant () sp id arg mcb


pattern PInterfaceDecl sp mod id tp ct ib = InterfaceDecl () sp mod id tp ct ib

pattern PInterfaceBody sp md = InterfaceBody () sp md

pattern PMemberDecl sp md = MemberDecl () sp md
pattern PInitDecl sp b bl = InitDecl () sp b bl

pattern PFieldDecl sp mod t vd = FieldDecl () sp mod t vd
pattern PMethodDecl sp mod tp rt id fp es mb = MethodDecl () sp mod tp rt id fp es mb
pattern PConstructorDecl sp mod tp id fp es cb = ConstructorDecl () sp mod tp id fp es cb
pattern PMemberClassDecl sp cd = MemberClassDecl () sp cd
pattern PMemberInterfaceDecl sp id = MemberInterfaceDecl () sp id
pattern PLockDecl sp mod id rt mlp = LockDecl () sp mod id rt mlp


pattern PVarDecl sp vdi mvi = VarDecl () sp vdi mvi

pattern PVarId sp id = VarId () sp id
pattern PVarDeclArray sp vdi = VarDeclArray () sp vdi

pattern PInitExp sp ex = InitExp () sp ex
pattern PInitArray sp ai = InitArray () sp ai

pattern PFormalParam sp mod t b vdi = FormalParam () sp mod t b vdi

pattern PMethodBody sp mb = MethodBody () sp mb

pattern PConstructorBody sp meci bs = ConstructorBody () sp meci bs

pattern PThisInvoke sp nwta a = ThisInvoke () sp nwta a
pattern PSuperInvoke sp nwta a = SuperInvoke () sp nwta a
pattern PPrimarySuperInvoke sp e nwta a = PrimarySuperInvoke () sp e nwta a


pattern PPublic sp = Public () sp
pattern PPrivate sp = Private () sp
pattern PProtected sp = Protected () sp
pattern PAbstract sp = Abstract () sp
pattern PFinal sp = Final () sp
pattern PStatic sp = Static () sp
pattern PStrictFP sp = StrictFP () sp
pattern PTransient sp = Transient () sp
pattern PVolatile sp = Volatile () sp
pattern PNative sp = Native () sp

pattern PTypemethod sp = Typemethod () sp
pattern PReflexive sp = Reflexive () sp
pattern PTransitive sp = Transitive () sp
pattern PSymmetric sp = Symmetric () sp
pattern PReadonly sp = Readonly () sp
pattern PNotnull sp = Notnull () sp

pattern PReads sp p = Reads () sp p
pattern PWrites sp p = Writes () sp p
pattern POpens sp l = Opens () sp l
pattern PCloses sp l = Closes () sp l
pattern PExpects sp l = Expects () sp l

pattern PBlock sp bs = Block () sp bs

pattern PBlockStmt sp st = BlockStmt () sp st
pattern PLocalClass sp cd = LocalClass () sp cd
pattern PLocalVars sp mod t vd = LocalVars () sp mod t vd
pattern PLocalLock sp mod id rt mlp = LocalLock () sp mod id rt mlp

pattern PStmtBlock sp b = StmtBlock () sp b
pattern PIfThen sp e s = IfThen () sp e s
pattern PIfThenElse sp e s1 s2 = IfThenElse () sp e s1 s2
pattern PWhile sp e s = While () sp e s
pattern PBasicFor sp mfi me mes st = BasicFor () sp mfi me mes st
pattern PEnhancedFor sp mod t id e st = EnhancedFor () sp mod t id e st
pattern PEmpty sp = Empty () sp
pattern PExpStmt sp e = ExpStmt () sp e
pattern PAssert sp e me = Assert () sp e me
pattern PSwitch sp e sb = Switch () sp e sb
pattern PDo sp s e = Do () sp s e
pattern PBreak sp mid = Break () sp mid
pattern PContinue sp mid = Continue () sp mid
pattern PReturn sp me = Return () sp me
pattern PSynchronized sp e b = Synchronized () sp e b
pattern PThrow sp e = Throw () sp e
pattern PTry sp b c mb = Try () sp b c mb
pattern PLabeled sp id s = Labeled () sp id s
pattern POpen sp l = Open () sp l
pattern PClose sp l = Close () sp l
pattern POpenBlock sp l b = OpenBlock () sp l b
pattern PCloseBlock sp l b = CloseBlock () sp l b

pattern PCatch sp fp b = Catch () sp fp b

pattern PSwitchBlock sp sl bs = SwitchBlock () sp sl bs

pattern PSwitchCase sp e = SwitchCase () sp e
pattern PDefault sp = Default () sp
 
pattern PForLocalVars sp mod t vd = ForLocalVars () sp mod t vd
pattern PForInitExps sp e = ForInitExps () sp e

pattern PExceptionSpec sp mod et = ExceptionSpec () sp mod et

pattern PLit sp l = Lit () sp l
pattern PClassLit sp mt = ClassLit () sp mt
pattern PThis sp = This () sp
pattern PThisClass sp n = ThisClass () sp n
pattern PParen sp e = Paren () sp e
pattern PInstanceCreation sp ta ct a mcb = InstanceCreation () sp ta ct a mcb
pattern PQualInstanceCreation sp e ta id a mcb = QualInstanceCreation () sp e ta id a mcb
pattern PArrayCreate sp t emp mp = ArrayCreate () sp t emp mp
pattern PArrayCreateInit sp t mp ai = ArrayCreateInit () sp t mp ai
pattern PFieldAccess sp fa = FieldAccess () sp fa
pattern PMethodInv sp mi = MethodInv () sp mi
pattern PArrayAccess sp ai = ArrayAccess () sp ai
pattern PExpName sp n = ExpName () sp n
pattern PPostIncrement sp e = PostIncrement () sp e
pattern PPostDecrement sp e = PostDecrement () sp e
pattern PPreIncrement sp e = PreIncrement () sp e
pattern PPreDecrement sp e = PreDecrement () sp e
pattern PPrePlus sp e = PrePlus () sp e
pattern PPreMinus sp e = PreMinus () sp e
pattern PPreBitCompl sp e = PreBitCompl () sp e
pattern PPreNot sp e = PreNot () sp e
pattern PCast sp t e = Cast () sp t e
pattern PBinOp sp e1 o e2 = BinOp () sp e1 o e2
pattern PInstanceOf sp e rt = InstanceOf () sp e rt
pattern PCond sp e1 e2 e3 = Cond () sp e1 e2 e3
pattern PAssign sp l ao e = Assign () sp l ao e
pattern PPolicyExp sp pe = PolicyExp () sp pe
pattern PLockExp sp l = LockExp () sp l


pattern PInt     sp i = Int     () sp i
pattern PWord    sp i = Word    () sp i
pattern PFloat   sp d = Float   () sp d
pattern PDouble  sp d = Double  () sp d
pattern PBoolean sp b = Boolean () sp b
pattern PChar    sp c = Char    () sp c
pattern PString  sp s = String  () sp s
pattern PNull    sp   = Null    () sp
pattern PMult    sp   = Mult    () sp
pattern PDiv     sp   = Div     () sp
pattern PRem     sp   = Rem     () sp
pattern PAdd     sp   = Add     () sp
pattern PSub     sp   = Sub     () sp
pattern PLShift  sp   = LShift  () sp
pattern PRShift  sp   = RShift  () sp
pattern PRRShift sp   = RRShift () sp
pattern PLThan   sp   = LThan   () sp
pattern PGThan   sp   = GThan   () sp
pattern PLThanE  sp   = LThanE  () sp
pattern PGThanE  sp   = GThanE  () sp
pattern PEqual   sp   = Equal   () sp
pattern PNotEq   sp   = NotEq   () sp
pattern PAnd     sp   = And     () sp
pattern POr      sp   = Or      () sp
pattern PXor     sp   = Xor     () sp
pattern PCAnd    sp   = CAnd    () sp
pattern PCOr     sp   = COr     () sp

pattern PEqualA sp = EqualA () sp
pattern PMultA sp = MultA () sp
pattern PDivA sp = DivA () sp
pattern PRemA sp = RemA () sp
pattern PAddA sp = AddA () sp
pattern PSubA sp = SubA () sp
pattern PLShiftA sp = LShiftA () sp
pattern PRShiftA sp = RShiftA () sp
pattern PRRShiftA sp = RRShiftA () sp
pattern PAndA sp = AndA () sp
pattern PXorA sp = XorA () sp
pattern POrA sp = OrA () sp

pattern PNameLhs sp n = NameLhs () sp n
pattern PFieldLhs sp fa = FieldLhs () sp fa
pattern PArrayLhs sp ai = ArrayLhs () sp ai

pattern PArrayIndex sp e1 e2 = ArrayIndex () sp e1 e2

pattern PPrimaryFieldAccess sp e id = PrimaryFieldAccess () sp e id
pattern PSuperFieldAccess sp id = SuperFieldAccess () sp id
pattern PClassFieldAccess sp n id = ClassFieldAccess () sp n id

pattern PMethodCallOrLockQuery sp n a = MethodCallOrLockQuery () sp n a
pattern PPrimaryMethodCall sp e nwta id a = PrimaryMethodCall () sp e nwta id a
pattern PSuperMethodCall sp nwta id a = SuperMethodCall () sp nwta id a
pattern PClassMethodCall sp n nwta id a = ClassMethodCall () sp n nwta id a
pattern PTypeMethodCall sp n nwta i a = TypeMethodCall () sp n nwta i a

pattern PArrayInit sp vi = ArrayInit () sp vi

pattern PVoidType sp = VoidType () sp
pattern PLockType sp = LockType () sp
pattern PType sp t = Type () sp t

pattern PPrimType sp pt = PrimType () sp pt
pattern PRefType sp rt = RefType () sp rt

pattern PClassRefType sp ct = ClassRefType () sp ct
pattern PTypeVariable sp id = TypeVariable () sp id
pattern PArrayType sp t mp = ArrayType () sp t mp

pattern PClassType sp n ta = ClassType () sp n ta

pattern PWildcard sp mwb = Wildcard () sp mwb
pattern PActualArg sp nwta = ActualArg () sp nwta

pattern PActualName sp n = ActualName () sp n
pattern PActualType sp rt = ActualType () sp rt
pattern PActualExp sp e = ActualExp () sp e
pattern PActualLockState sp l = ActualLockState () sp l

pattern PExtendsBound sp rt = ExtendsBound () sp rt
pattern PSuperBound sp rt = SuperBound () sp rt

pattern PBooleanT sp = BooleanT () sp
pattern PByteT sp = ByteT () sp
pattern PShortT sp = ShortT () sp
pattern PIntT sp = IntT () sp
pattern PLongT sp = LongT () sp
pattern PCharT sp = CharT () sp
pattern PFloatT sp = FloatT () sp
pattern PDoubleT sp = DoubleT () sp
pattern PActorT sp = ActorT () sp
pattern PPolicyT sp = PolicyT () sp

pattern PActorParam sp rt id = ActorParam () sp rt id
pattern PPolicyParam sp id = PolicyParam () sp id
pattern PLockStateParam sp id = LockStateParam () sp id

pattern PPolicyLit sp c = PolicyLit () sp c
pattern PPolicyOf sp id = PolicyOf () sp id
pattern PPolicyThis sp = PolicyThis () sp
pattern PPolicyTypeVar sp id = PolicyTypeVar () sp id



pattern PLockProperties sp lc = LockProperties () sp lc

pattern PClause sp cvd ch a = Clause () sp cvd ch a

pattern PClauseVarDecl sp rt i = ClauseVarDecl () sp rt i

pattern PClauseDeclHead sp cvd = ClauseDeclHead () sp cvd
pattern PClauseVarHead sp a = ClauseVarHead () sp a

pattern PLClause sp cvd a1 a2 = LClause () sp cvd a1 a2
pattern PConstraintClause sp cvd a = ConstraintClause () sp cvd a

pattern PActor sp an = Actor () sp an
pattern PVar sp id = Var () sp id

pattern PActorName sp n = ActorName () sp n
pattern PActorTypeVar sp rt id = ActorTypeVar () sp rt id

pattern PAtom sp n a = Atom () sp n a

pattern PLock sp n an = Lock () sp n an
pattern PLockVar sp id = LockVar () sp id

pattern PIdent sp bs = Ident () sp bs

  
-- AntiQStuff
pattern PAntiQIdent sp s = AntiQIdent () sp s
pattern PAntiQName  sp s = AntiQName  () sp s
pattern PAntiQType  sp s = AntiQType  () sp s
pattern PAntiQExp   sp s = AntiQExp   () sp s

-- FormalParam
-- LockType
-- FieldAccess
-- MethodInv
-- ArrayAccess
-- ClassRefType
-- ActualArg
-- ClauseVarDecl
-- ClassDecl
-- ClassRefType
-- Name
-- ActorName
-- ExpName
-- Lock
-- TypeParam
-- LockStateParam
-- PolicyParam
-- ActorParam
-- Actor
-- Mult
-- BinOp
-- FormalParam
-- LockType
-- FieldAccess
-- MethodInv
-- ArrayAccess

--------------------------------------------------------------------------------
-- NameResolution (NR)
--------------------------------------------------------------------------------
data NR

type instance XSP                  NR = SourcePos
type instance XCompilationUnit     NR = NoFieldExt
type instance XPackageDecl         NR = NoFieldExt
type instance XImportDecl          NR = NoFieldExt
type instance XTypeDecl            NR = NoFieldExt
type instance XClassDecl           NR = NoFieldExt
type instance XClassBody           NR = NoFieldExt
type instance XEnumBody            NR = NoFieldExt
type instance XEnumConstant        NR = NoFieldExt
type instance XInterfaceDecl       NR = NoFieldExt
type instance XInterfaceBody       NR = NoFieldExt
type instance XDecl                NR = NoFieldExt
type instance XMemberDecl          NR = NoFieldExt
type instance XVarDecl             NR = NoFieldExt
type instance XVarDeclId           NR = NoFieldExt
-- type instance XInitExp             NR = NoFieldExt
type instance XFormalParam         NR = NoFieldExt
type instance XMethodBody          NR = NoFieldExt
type instance XConstructorBody     NR = NoFieldExt
type instance XExplConstrInv       NR = NoFieldExt
type instance XMod                 NR = NoFieldExt
type instance XBlock               NR = NoFieldExt
type instance XBlockStm            NR = NoFieldExt
type instance XStm                 NR = NoFieldExt
type instance XCatch               NR = NoFieldExt
type instance XSwitchBlock         NR = NoFieldExt
type instance XSwitchLabel         NR = NoFieldExt
type instance XForInit             NR = NoFieldExt
type instance XExceptionSpec       NR = NoFieldExt
type instance XExp                 NR = NoFieldExt
type instance XLiteral             NR = NoFieldExt
type instance XOp                  NR = NoFieldExt
type instance XAssignOp            NR = NoFieldExt
type instance XLhs                 NR = NoFieldExt
type instance XArrayIndex          NR = NoFieldExt
type instance XFieldAccess         NR = NoFieldExt
type instance XMethodInvocation    NR = NoFieldExt
type instance XArrayInit           NR = NoFieldExt
type instance XReturnType          NR = NoFieldExt
type instance XType                NR = NoFieldExt
type instance XRefType             NR = NoFieldExt
type instance XClassType           NR = NoFieldExt
type instance XTypeArgument        NR = NoFieldExt
type instance XNonWildTypeArgument NR = NoFieldExt
type instance XWildcardBound       NR = NoFieldExt
type instance XPrimType            NR = NoFieldExt
type instance XTypeParam           NR = NoFieldExt
type instance XPolicyExp           NR = NoFieldExt
type instance XLockProperties      NR = NoFieldExt
type instance XClause              NR = NoFieldExt
type instance XClauseVarDecl       NR = NoFieldExt
type instance XClauseHead          NR = NoFieldExt
type instance XLClause             NR = NoFieldExt
type instance XActor               NR = NoFieldExt
type instance XActorName           NR = NoFieldExt
type instance XAtom                NR = NoFieldExt
type instance XLock                NR = NoFieldExt
type instance XIdent               NR = NoFieldExt

--------------------------------------------------------------------------------
-- TypeCheck (TC)
--------------------------------------------------------------------------------
data TC
type TcPlaceHolder = ()

-- data TcType = PrimT TcPrimT | RefT TcRefType

type instance XSP                  TC = SourcePos
type instance XCompilationUnit     TC = NoFieldExt
type instance XPackageDecl         TC = NoFieldExt
type instance XImportDecl          TC = NoFieldExt
type instance XTypeDecl            TC = NoFieldExt
type instance XClassDecl           TC = NoFieldExt
type instance XClassBody           TC = NoFieldExt
type instance XEnumBody            TC = NoFieldExt
type instance XEnumConstant        TC = NoFieldExt
type instance XInterfaceDecl       TC = NoFieldExt
type instance XInterfaceBody       TC = NoFieldExt
type instance XDecl                TC = NoFieldExt
type instance XMemberDecl          TC = TcPlaceHolder
type instance XVarDecl             TC = TcPlaceHolder
type instance XVarDeclId           TC = TcPlaceHolder
-- type instance XInitExp             TC = TcPlaceHolder
type instance XFormalParam         TC = TcPlaceHolder
type instance XMethodBody          TC = TcPlaceHolder
type instance XConstructorBody     TC = TcPlaceHolder
type instance XExplConstrInv       TC = TcPlaceHolder
type instance XMod                 TC = TcPlaceHolder
type instance XBlock               TC = TcPlaceHolder
type instance XBlockStm            TC = TcPlaceHolder
type instance XStm                 TC = TcPlaceHolder
type instance XCatch               TC = TcPlaceHolder
type instance XSwitchBlock         TC = TcPlaceHolder
type instance XSwitchLabel         TC = TcPlaceHolder
type instance XForInit             TC = TcPlaceHolder
type instance XExceptionSpec       TC = TcPlaceHolder
type instance XExp                 TC = TcPlaceHolder
type instance XLiteral             TC = TcPlaceHolder
type instance XOp                  TC = TcPlaceHolder
type instance XAssignOp            TC = TcPlaceHolder
type instance XLhs                 TC = TcPlaceHolder
type instance XArrayIndex          TC = TcPlaceHolder
type instance XFieldAccess         TC = TcPlaceHolder
type instance XMethodInvocation    TC = TcPlaceHolder
type instance XArrayInit           TC = TcPlaceHolder
type instance XReturnType          TC = TcPlaceHolder
type instance XType                TC = TcPlaceHolder
type instance XRefType             TC = TcPlaceHolder
type instance XClassType           TC = TcPlaceHolder
type instance XTypeArgument        TC = TcPlaceHolder
type instance XNonWildTypeArgument TC = TcPlaceHolder
type instance XWildcardBound       TC = TcPlaceHolder
type instance XPrimType            TC = TcPlaceHolder
type instance XTypeParam           TC = TcPlaceHolder
type instance XPolicyExp           TC = TcPlaceHolder
type instance XLockProperties      TC = TcPlaceHolder
type instance XClause              TC = TcPlaceHolder
type instance XClauseVarDecl       TC = TcPlaceHolder
type instance XClauseHead          TC = TcPlaceHolder
type instance XLClause             TC = TcPlaceHolder
type instance XActor               TC = TcPlaceHolder
type instance XActorName           TC = TcPlaceHolder
type instance XAtom                TC = TcPlaceHolder
type instance XLock                TC = TcPlaceHolder
type instance XIdent               TC = TcPlaceHolder

--------------------------------------------------------------------------------
-- PolicyTypeEval (PTE)
--------------------------------------------------------------------------------

data PTE
type PtePlaceHolder = ()

type instance XSP                  PTE = SourcePos
type instance XCompilationUnit     PTE = PtePlaceHolder
type instance XPackageDecl         PTE = PtePlaceHolder
type instance XImportDecl          PTE = PtePlaceHolder
type instance XTypeDecl            PTE = PtePlaceHolder
type instance XClassDecl           PTE = PtePlaceHolder
type instance XClassBody           PTE = PtePlaceHolder
type instance XEnumBody            PTE = PtePlaceHolder
type instance XEnumConstant        PTE = PtePlaceHolder
type instance XInterfaceDecl       PTE = PtePlaceHolder
type instance XInterfaceBody       PTE = PtePlaceHolder
type instance XDecl                PTE = PtePlaceHolder
type instance XMemberDecl          PTE = PtePlaceHolder
type instance XVarDecl             PTE = PtePlaceHolder
type instance XVarDeclId           PTE = PtePlaceHolder
-- type instance XInitExp             PTE = PtePlaceHolder
type instance XFormalParam         PTE = PtePlaceHolder
type instance XMethodBody          PTE = PtePlaceHolder
type instance XConstructorBody     PTE = PtePlaceHolder
type instance XExplConstrInv       PTE = PtePlaceHolder
type instance XMod                 PTE = PtePlaceHolder
type instance XBlock               PTE = PtePlaceHolder
type instance XBlockStm            PTE = PtePlaceHolder
type instance XStm                 PTE = PtePlaceHolder
type instance XCatch               PTE = PtePlaceHolder
type instance XSwitchBlock         PTE = PtePlaceHolder
type instance XSwitchLabel         PTE = PtePlaceHolder
type instance XForInit             PTE = PtePlaceHolder
type instance XExceptionSpec       PTE = PtePlaceHolder
type instance XExp                 PTE = PtePlaceHolder
type instance XLiteral             PTE = PtePlaceHolder
type instance XOp                  PTE = PtePlaceHolder
type instance XAssignOp            PTE = PtePlaceHolder
type instance XLhs                 PTE = PtePlaceHolder
type instance XArrayIndex          PTE = PtePlaceHolder
type instance XFieldAccess         PTE = PtePlaceHolder
type instance XMethodInvocation    PTE = PtePlaceHolder
type instance XArrayInit           PTE = PtePlaceHolder
type instance XReturnType          PTE = PtePlaceHolder
type instance XType                PTE = PtePlaceHolder
type instance XRefType             PTE = PtePlaceHolder
type instance XClassType           PTE = PtePlaceHolder
type instance XTypeArgument        PTE = PtePlaceHolder
type instance XNonWildTypeArgument PTE = PtePlaceHolder
type instance XWildcardBound       PTE = PtePlaceHolder
type instance XPrimType            PTE = PtePlaceHolder
type instance XTypeParam           PTE = PtePlaceHolder
type instance XPolicyExp           PTE = PtePlaceHolder
type instance XLockProperties      PTE = PtePlaceHolder
type instance XClause              PTE = PtePlaceHolder
type instance XClauseVarDecl       PTE = PtePlaceHolder
type instance XClauseHead          PTE = PtePlaceHolder
type instance XLClause             PTE = PtePlaceHolder
type instance XActor               PTE = PtePlaceHolder
type instance XActorName           PTE = PtePlaceHolder
type instance XAtom                PTE = PtePlaceHolder
type instance XLock                PTE = PtePlaceHolder
type instance XIdent               PTE = PtePlaceHolder
--------------------------------------------------------------------------------
-- LockStateEval (LSE)
--------------------------------------------------------------------------------

data LSE
type LsePlaceHolder = ()

type instance XSP                  LSE = SourcePos
type instance XCompilationUnit     LSE = LsePlaceHolder
type instance XPackageDecl         LSE = LsePlaceHolder
type instance XImportDecl          LSE = LsePlaceHolder
type instance XTypeDecl            LSE = LsePlaceHolder
type instance XClassDecl           LSE = LsePlaceHolder
type instance XClassBody           LSE = LsePlaceHolder
type instance XEnumBody            LSE = LsePlaceHolder
type instance XEnumConstant        LSE = LsePlaceHolder
type instance XInterfaceDecl       LSE = LsePlaceHolder
type instance XInterfaceBody       LSE = LsePlaceHolder
type instance XDecl                LSE = LsePlaceHolder
type instance XMemberDecl          LSE = LsePlaceHolder
type instance XVarDecl             LSE = LsePlaceHolder
type instance XVarDeclId           LSE = LsePlaceHolder
-- type instance XInitExp             LSE = LsePlaceHolder
type instance XFormalParam         LSE = LsePlaceHolder
type instance XMethodBody          LSE = LsePlaceHolder
type instance XConstructorBody     LSE = LsePlaceHolder
type instance XExplConstrInv       LSE = LsePlaceHolder
type instance XMod                 LSE = LsePlaceHolder
type instance XBlock               LSE = LsePlaceHolder
type instance XBlockStm            LSE = LsePlaceHolder
type instance XStm                 LSE = LsePlaceHolder
type instance XCatch               LSE = LsePlaceHolder
type instance XSwitchBlock         LSE = LsePlaceHolder
type instance XSwitchLabel         LSE = LsePlaceHolder
type instance XForInit             LSE = LsePlaceHolder
type instance XExceptionSpec       LSE = LsePlaceHolder
type instance XExp                 LSE = LsePlaceHolder
type instance XLiteral             LSE = LsePlaceHolder
type instance XOp                  LSE = LsePlaceHolder
type instance XAssignOp            LSE = LsePlaceHolder
type instance XLhs                 LSE = LsePlaceHolder
type instance XArrayIndex          LSE = LsePlaceHolder
type instance XFieldAccess         LSE = LsePlaceHolder
type instance XMethodInvocation    LSE = LsePlaceHolder
type instance XArrayInit           LSE = LsePlaceHolder
type instance XReturnType          LSE = LsePlaceHolder
type instance XType                LSE = LsePlaceHolder
type instance XRefType             LSE = LsePlaceHolder
type instance XClassType           LSE = LsePlaceHolder
type instance XTypeArgument        LSE = LsePlaceHolder
type instance XNonWildTypeArgument LSE = LsePlaceHolder
type instance XWildcardBound       LSE = LsePlaceHolder
type instance XPrimType            LSE = LsePlaceHolder
type instance XTypeParam           LSE = LsePlaceHolder
type instance XPolicyExp           LSE = LsePlaceHolder
type instance XLockProperties      LSE = LsePlaceHolder
type instance XClause              LSE = LsePlaceHolder
type instance XClauseVarDecl       LSE = LsePlaceHolder
type instance XClauseHead          LSE = LsePlaceHolder
type instance XLClause             LSE = LsePlaceHolder
type instance XActor               LSE = LsePlaceHolder
type instance XActorName           LSE = LsePlaceHolder
type instance XAtom                LSE = LsePlaceHolder
type instance XLock                LSE = LsePlaceHolder
type instance XIdent               LSE = LsePlaceHolder


--------------------------------------------------------------------------------
-- PolicyConstraintGen (PCG)
--------------------------------------------------------------------------------

data PCG
type PcgPlaceHolder = ()

type instance XSP                  PCG = SourcePos
type instance XCompilationUnit     PCG = PcgPlaceHolder
type instance XPackageDecl         PCG = PcgPlaceHolder
type instance XImportDecl          PCG = PcgPlaceHolder
type instance XTypeDecl            PCG = PcgPlaceHolder
type instance XClassDecl           PCG = PcgPlaceHolder
type instance XClassBody           PCG = PcgPlaceHolder
type instance XEnumBody            PCG = PcgPlaceHolder
type instance XEnumConstant        PCG = PcgPlaceHolder
type instance XInterfaceDecl       PCG = PcgPlaceHolder
type instance XInterfaceBody       PCG = PcgPlaceHolder
type instance XDecl                PCG = PcgPlaceHolder
type instance XMemberDecl          PCG = PcgPlaceHolder
type instance XVarDecl             PCG = PcgPlaceHolder
type instance XVarDeclId           PCG = PcgPlaceHolder
-- type instance XInitExp             PCG = PcgPlaceHolder
type instance XFormalParam         PCG = PcgPlaceHolder
type instance XMethodBody          PCG = PcgPlaceHolder
type instance XConstructorBody     PCG = PcgPlaceHolder
type instance XExplConstrInv       PCG = PcgPlaceHolder
type instance XMod                 PCG = PcgPlaceHolder
type instance XBlock               PCG = PcgPlaceHolder
type instance XBlockStm            PCG = PcgPlaceHolder
type instance XStm                 PCG = PcgPlaceHolder
type instance XCatch               PCG = PcgPlaceHolder
type instance XSwitchBlock         PCG = PcgPlaceHolder
type instance XSwitchLabel         PCG = PcgPlaceHolder
type instance XForInit             PCG = PcgPlaceHolder
type instance XExceptionSpec       PCG = PcgPlaceHolder
type instance XExp                 PCG = PcgPlaceHolder
type instance XLiteral             PCG = PcgPlaceHolder
type instance XOp                  PCG = PcgPlaceHolder
type instance XAssignOp            PCG = PcgPlaceHolder
type instance XLhs                 PCG = PcgPlaceHolder
type instance XArrayIndex          PCG = PcgPlaceHolder
type instance XFieldAccess         PCG = PcgPlaceHolder
type instance XMethodInvocation    PCG = PcgPlaceHolder
type instance XArrayInit           PCG = PcgPlaceHolder
type instance XReturnType          PCG = PcgPlaceHolder
type instance XType                PCG = PcgPlaceHolder
type instance XRefType             PCG = PcgPlaceHolder
type instance XClassType           PCG = PcgPlaceHolder
type instance XTypeArgument        PCG = PcgPlaceHolder
type instance XNonWildTypeArgument PCG = PcgPlaceHolder
type instance XWildcardBound       PCG = PcgPlaceHolder
type instance XPrimType            PCG = PcgPlaceHolder
type instance XTypeParam           PCG = PcgPlaceHolder
type instance XPolicyExp           PCG = PcgPlaceHolder
type instance XLockProperties      PCG = PcgPlaceHolder
type instance XClause              PCG = PcgPlaceHolder
type instance XClauseVarDecl       PCG = PcgPlaceHolder
type instance XClauseHead          PCG = PcgPlaceHolder
type instance XLClause             PCG = PcgPlaceHolder
type instance XActor               PCG = PcgPlaceHolder
type instance XActorName           PCG = PcgPlaceHolder
type instance XAtom                PCG = PcgPlaceHolder
type instance XLock                PCG = PcgPlaceHolder
type instance XIdent               PCG = PcgPlaceHolder

--------------------------------------------------------------------------------
-- PolicyConstraintSolver (PCS)
--------------------------------------------------------------------------------

data PCS
type PcsPlaceHolder = ()

type instance XSP                  PCS = SourcePos
type instance XCompilationUnit     PCS = PcsPlaceHolder
type instance XPackageDecl         PCS = PcsPlaceHolder
type instance XImportDecl          PCS = PcsPlaceHolder
type instance XTypeDecl            PCS = PcsPlaceHolder
type instance XClassDecl           PCS = PcsPlaceHolder
type instance XClassBody           PCS = PcsPlaceHolder
type instance XEnumBody            PCS = PcsPlaceHolder
type instance XEnumConstant        PCS = PcsPlaceHolder
type instance XInterfaceDecl       PCS = PcsPlaceHolder
type instance XInterfaceBody       PCS = PcsPlaceHolder
type instance XDecl                PCS = PcsPlaceHolder
type instance XMemberDecl          PCS = PcsPlaceHolder
type instance XVarDecl             PCS = PcsPlaceHolder
type instance XVarDeclId           PCS = PcsPlaceHolder
-- type instance XInitExp             PCS = PcsPlaceHolder
type instance XFormalParam         PCS = PcsPlaceHolder
type instance XMethodBody          PCS = PcsPlaceHolder
type instance XConstructorBody     PCS = PcsPlaceHolder
type instance XExplConstrInv       PCS = PcsPlaceHolder
type instance XMod                 PCS = PcsPlaceHolder
type instance XBlock               PCS = PcsPlaceHolder
type instance XBlockStm            PCS = PcsPlaceHolder
type instance XStm                 PCS = PcsPlaceHolder
type instance XCatch               PCS = PcsPlaceHolder
type instance XSwitchBlock         PCS = PcsPlaceHolder
type instance XSwitchLabel         PCS = PcsPlaceHolder
type instance XForInit             PCS = PcsPlaceHolder
type instance XExceptionSpec       PCS = PcsPlaceHolder
type instance XExp                 PCS = PcsPlaceHolder
type instance XLiteral             PCS = PcsPlaceHolder
type instance XOp                  PCS = PcsPlaceHolder
type instance XAssignOp            PCS = PcsPlaceHolder
type instance XLhs                 PCS = PcsPlaceHolder
type instance XArrayIndex          PCS = PcsPlaceHolder
type instance XFieldAccess         PCS = PcsPlaceHolder
type instance XMethodInvocation    PCS = PcsPlaceHolder
type instance XArrayInit           PCS = PcsPlaceHolder
type instance XReturnType          PCS = PcsPlaceHolder
type instance XType                PCS = PcsPlaceHolder
type instance XRefType             PCS = PcsPlaceHolder
type instance XClassType           PCS = PcsPlaceHolder
type instance XTypeArgument        PCS = PcsPlaceHolder
type instance XNonWildTypeArgument PCS = PcsPlaceHolder
type instance XWildcardBound       PCS = PcsPlaceHolder
type instance XPrimType            PCS = PcsPlaceHolder
type instance XTypeParam           PCS = PcsPlaceHolder
type instance XPolicyExp           PCS = PcsPlaceHolder
type instance XLockProperties      PCS = PcsPlaceHolder
type instance XClause              PCS = PcsPlaceHolder
type instance XClauseVarDecl       PCS = PcsPlaceHolder
type instance XClauseHead          PCS = PcsPlaceHolder
type instance XLClause             PCS = PcsPlaceHolder
type instance XActor               PCS = PcsPlaceHolder
type instance XActorName           PCS = PcsPlaceHolder
type instance XAtom                PCS = PcsPlaceHolder
type instance XLock                PCS = PcsPlaceHolder
type instance XIdent               PCS = PcsPlaceHolder


-- Unit instances (hack-solution?)
type instance XSP () = ()
type instance XOp () = ()


--------------------------------------------------------------------------------
-- Since the new Syntax file does not compile, the type families are replicated
-- here for now.
--------------------------------------------------------------------------------
-- type family XSP                  x
-- type family XCompilationUnit     x
-- type family XPackageDecl         x
-- type family XImportDecl          x
-- type family XTypeDecl            x
-- type family XClassDecl           x
-- type family XClassBody           x
-- type family XEnumBody            x
-- type family XEnumConstant        x
-- type family XInterfaceDecl       x
-- type family XInterfaceBody       x
-- type family XDecl                x
-- type family XMemberDecl          x
-- type family XVarDecl             x
-- type family XVarDeclId           x
-- type family XInitExp             x
-- type family XFormalParam         x
-- type family XMethodBody          x
-- type family XConstructorBody     x
-- type family XExplConstrInv       x
-- type family XMod                 x
-- type family XBlock               x
-- type family XBlockStm            x
-- type family XStm                 x
-- type family XCatch               x
-- type family XSwitchBlock         x
-- type family XSwitchLabel         x
-- type family XForInit             x
-- type family XExceptionSpec       x
-- type family XExp                 x
-- type family XLiteral             x
-- type family XOp                  x
-- type family XAssignOp            x
-- type family XLhs                 x
-- type family XArrayIndex          x
-- type family XFieldAccess         x
-- type family XMethodInvocation    x
-- type family XArrayInit           x
-- type family XReturnType          x
-- type family XType                x
-- type family XRefType             x
-- type family XClassType           x
-- type family XTypeArgument        x
-- type family XNonWildTypeArgument x
-- type family XWildcardBound       x
-- type family XPrimType            x
-- type family XTypeParam           x
-- type family XPolicyExp           x
-- type family XLockProperties      x
-- type family XClause              x
-- type family XClauseVarDecl       x
-- type family XClauseHead          x
-- type family XLClause             x
-- type family XActor               x
-- type family XActorName           x
-- type family XAtom                x
-- type family XLock                x
-- type family XIdent               x
