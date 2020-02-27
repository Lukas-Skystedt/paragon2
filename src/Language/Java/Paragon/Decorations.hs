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

type instance XSP                  PCS = NoFieldExt
type instance XCompilationUnit     PCS = NoFieldExt
type instance XPackageDecl         PCS = NoFieldExt
type instance XImportDecl          PCS = NoFieldExt
type instance XTypeDecl            PCS = NoFieldExt
type instance XClassDecl           PCS = NoFieldExt
type instance XClassBody           PCS = NoFieldExt
type instance XEnumBody            PCS = NoFieldExt
type instance XEnumConstant        PCS = NoFieldExt
type instance XInterfaceDecl       PCS = NoFieldExt
type instance XInterfaceBody       PCS = NoFieldExt
type instance XDecl                PCS = NoFieldExt
type instance XMemberDecl          PCS = NoFieldExt
type instance XVarDecl             PCS = NoFieldExt
type instance XVarDeclId           PCS = NoFieldExt
type instance XFormalParam         PCS = NoFieldExt
type instance XMethodBody          PCS = NoFieldExt
type instance XConstructorBody     PCS = NoFieldExt
type instance XExplConstrInv       PCS = NoFieldExt
type instance XMod                 PCS = NoFieldExt
type instance XBlock               PCS = NoFieldExt
type instance XBlockStm            PCS = NoFieldExt
type instance XStm                 PCS = NoFieldExt
type instance XCatch               PCS = NoFieldExt
type instance XSwitchBlock         PCS = NoFieldExt
type instance XSwitchLabel         PCS = NoFieldExt
type instance XForInit             PCS = NoFieldExt
type instance XExceptionSpec       PCS = NoFieldExt
type instance XExp                 PCS = NoFieldExt
type instance XLiteral             PCS = NoFieldExt
type instance XOp                  PCS = NoFieldExt
type instance XAssignOp            PCS = NoFieldExt
type instance XLhs                 PCS = NoFieldExt
type instance XArrayIndex          PCS = NoFieldExt
type instance XFieldAccess         PCS = NoFieldExt
type instance XMethodInvocation    PCS = NoFieldExt
type instance XArrayInit           PCS = NoFieldExt
type instance XReturnType          PCS = NoFieldExt
type instance XType                PCS = NoFieldExt
type instance XRefType             PCS = NoFieldExt
type instance XClassType           PCS = NoFieldExt
type instance XTypeArgument        PCS = NoFieldExt
type instance XNonWildTypeArgument PCS = NoFieldExt
type instance XWildcardBound       PCS = NoFieldExt
type instance XPrimType            PCS = NoFieldExt
type instance XTypeParam           PCS = NoFieldExt
type instance XPolicyExp           PCS = NoFieldExt
type instance XLockProperties      PCS = NoFieldExt
type instance XClause              PCS = NoFieldExt
type instance XClauseVarDecl       PCS = NoFieldExt
type instance XClauseHead          PCS = NoFieldExt
type instance XLClause             PCS = NoFieldExt
type instance XActor               PCS = NoFieldExt
type instance XActorName           PCS = NoFieldExt
type instance XAtom                PCS = NoFieldExt
type instance XLock                PCS = NoFieldExt
type instance XIdent               PCS = NoFieldExt

pattern PcsTypeParam id re = TypeParam () () id re

pattern PcsCompilationUnit mpd id td = CompilationUnit () () mpd id td

pattern PcsPackageDecl n = PackageDecl () () n

pattern PcsSingleTypeImport n = SingleTypeImport () () n
pattern PcsTypeImportOnDemand n = TypeImportOnDemand () () n
pattern PcsSingleStaticImport n id = SingleStaticImport () () n id
pattern PcsStaticImportOnDemand n = StaticImportOnDemand () () n


pattern PcsClassTypeDecl cl = ClassTypeDecl () () cl
pattern PcsInterfaceTypeDecl id = InterfaceTypeDecl () () id

pattern PcsClassDecl mod id tp mct ct cb = ClassDecl () () mod id tp mct ct cb
pattern PcsEnumDecl mod id ct eb = EnumDecl () () mod id ct eb

pattern PcsClassBody d = ClassBody () () d

pattern PcsEnumBody ec d = EnumBody () () ec d

pattern PcsEnumConstant id arg mcb = EnumConstant () () id arg mcb


pattern PcsInterfaceDecl mod id tp ct ib = InterfaceDecl () () mod id tp ct ib

pattern PcsInterfaceBody md = InterfaceBody () () md

pattern PcsMemberDecl md = MemberDecl () () md
pattern PcsInitDecl b bl = InitDecl () () b bl

pattern PcsFieldDecl mod t vd = FieldDecl () () mod t vd
pattern PcsMethodDecl mod tp rt id fp es mb = MethodDecl () () mod tp rt id fp es mb
pattern PcsConstructorDecl mod tp id fp es cb = ConstructorDecl () () mod tp id fp es cb
pattern PcsMemberClassDecl cd = MemberClassDecl () () cd
pattern PcsMemberInterfaceDecl id = MemberInterfaceDecl () () id
pattern PcsLockDecl mod id rt mlp = LockDecl () () mod id rt mlp


pattern PcsVarDecl vdi mvi = VarDecl () () vdi mvi

pattern PcsVarId id = VarId () () id
pattern PcsVarDeclArray vdi = VarDeclArray () () vdi

pattern PcsInitExp ex = InitExp () () ex
pattern PcsInitArray ai = InitArray () () ai

pattern PcsFormalParam mod t b vdi = FormalParam () () mod t b vdi

pattern PcsMethodBody mb = MethodBody () () mb

pattern PcsConstructorBody meci bs = ConstructorBody () () meci bs

pattern PcsThisInvoke nwta a = ThisInvoke () () nwta a
pattern PcsSuperInvoke nwta a = SuperInvoke () () nwta a
pattern PcsPrimarySuperInvoke e nwta a = PrimarySuperInvoke () () e nwta a


pattern PcsPublic = Public () ()
pattern PcsPrivate = Private () ()
pattern PcsProtected = Protected () ()
pattern PcsAbstract = Abstract () ()
pattern PcsFinal = Final () ()
pattern PcsStatic = Static () ()
pattern PcsStrictFP = StrictFP () ()
pattern PcsTransient = Transient () ()
pattern PcsVolatile = Volatile () ()
pattern PcsNative = Native () ()

pattern PcsTypemethod = Typemethod () ()
pattern PcsReflexive = Reflexive () ()
pattern PcsTransitive = Transitive () ()
pattern PcsSymmetric = Symmetric () ()
pattern PcsReadonly = Readonly () ()
pattern PcsNotnull = Notnull () ()

pattern PcsReads p = Reads () () p
pattern PcsWrites p = Writes () () p
pattern PcsOpens l = Opens () () l
pattern PcsCloses l = Closes () () l
pattern PcsExpects l = Expects () () l

pattern PcsBlock bs = Block () () bs

pattern PcsBlockStmt st = BlockStmt () () st
pattern PcsLocalClass cd = LocalClass () () cd
pattern PcsLocalVars mod t vd = LocalVars () () mod t vd
pattern PcsLocalLock mod id rt mlp = LocalLock () () mod id rt mlp

pattern PcsStmtBlock b = StmtBlock () () b
pattern PcsIfThen e s = IfThen () () e s
pattern PcsIfThenElse e s1 s2 = IfThenElse () () e s1 s2
pattern PcsWhile e s = While () () e s
pattern PcsBasicFor mfi me mes st = BasicFor () () mfi me mes st
pattern PcsEnhancedFor mod t id e st = EnhancedFor () () mod t id e st
pattern PcsEmpty = Empty () ()
pattern PcsExpStmt e = ExpStmt () () e
pattern PcsAssert e me = Assert () () e me
pattern PcsSwitch e sb = Switch () () e sb
pattern PcsDo s e = Do () () s e
pattern PcsBreak mid = Break () () mid
pattern PcsContinue mid = Continue () () mid
pattern PcsReturn me = Return () () me
pattern PcsSynchronized e b = Synchronized () () e b
pattern PcsThrow e = Throw () () e
pattern PcsTry b c mb = Try () () b c mb
pattern PcsLabeled id s = Labeled () () id s
pattern PcsOpen l = Open () () l
pattern PcsClose l = Close () () l
pattern PcsOpenBlock l b = OpenBlock () () l b
pattern PcsCloseBlock l b = CloseBlock () () l b

pattern PcsCatch fp b = Catch () () fp b

pattern PcsSwitchBlock sl bs = SwitchBlock () () sl bs

pattern PcsSwitchCase e = SwitchCase () () e
pattern PcsDefault = Default () ()

pattern PcsForLocalVars mod t vd = ForLocalVars () () mod t vd
pattern PcsForInitExps e = ForInitExps () () e

pattern PcsExceptionSpec mod et = ExceptionSpec () () mod et

pattern PcsLit l = Lit () () l
pattern PcsClassLit mt = ClassLit () () mt
pattern PcsThis = This () ()
pattern PcsThisClass n = ThisClass () () n
pattern PcsParen e = Paren () () e
pattern PcsInstanceCreation ta ct a mcb = InstanceCreation () () ta ct a mcb
pattern PcsQualInstanceCreation e ta id a mcb = QualInstanceCreation () () e ta id a mcb
pattern PcsArrayCreate t emp mp = ArrayCreate () () t emp mp
pattern PcsArrayCreateInit t mp ai = ArrayCreateInit () () t mp ai
pattern PcsFieldAccess fa = FieldAccess () () fa
pattern PcsMethodInv mi = MethodInv () () mi
pattern PcsArrayAccess ai = ArrayAccess () () ai
pattern PcsExpName n = ExpName () () n
pattern PcsPostIncrement e = PostIncrement () () e
pattern PcsPostDecrement e = PostDecrement () () e
pattern PcsPreIncrement e = PreIncrement () () e
pattern PcsPreDecrement e = PreDecrement () () e
pattern PcsPrePlus e = PrePlus () () e
pattern PcsPreMinus e = PreMinus () () e
pattern PcsPreBitCompl e = PreBitCompl () () e
pattern PcsPreNot e = PreNot () () e
pattern PcsCast t e = Cast () () t e
pattern PcsBinOp e1 o e2 = BinOp () () e1 o e2
pattern PcsInstanceOf e rt = InstanceOf () () e rt
pattern PcsCond e1 e2 e3 = Cond () () e1 e2 e3
pattern PcsAssign l ao e = Assign () () l ao e
pattern PcsPolicyExp pe = PolicyExp () () pe
pattern PcsLockExp l = LockExp () () l

pattern PcsInt i = Int () () i
pattern PcsWord i = Word () () i
pattern PcsFloat d = Float () () d
pattern PcsDouble d = Double () () d
pattern PcsBoolean b = Boolean () () b
pattern PcsChar c = Char () () c
pattern PcsString s = String () () s
pattern PcsNull = Null () ()

pattern PcsMult = Mult () ()
pattern PcsDiv = Div () ()
pattern PcsRem = Rem () ()
pattern PcsAdd = Add () ()
pattern PcsSub = Sub () ()
pattern PcsLShift = LShift () ()
pattern PcsRShift = RShift () ()
pattern PcsRRShift = RRShift () ()
pattern PcsLThan = LThan () ()
pattern PcsGThan = GThan () ()
pattern PcsLThanE = LThanE () ()
pattern PcsGThanE = GThanE () ()
pattern PcsEqual = Equal () ()
pattern PcsNotEq = NotEq () ()
pattern PcsAnd = And () ()
pattern PcsOr = Or () ()
pattern PcsXor = Xor () ()
pattern PcsCAnd = CAnd () ()
pattern PcsCOr = COr () ()

pattern PcsEqualA = EqualA () ()
pattern PcsMultA = MultA () ()
pattern PcsDivA = DivA () ()
pattern PcsRemA = RemA () ()
pattern PcsAddA = AddA () ()
pattern PcsSubA = SubA () ()
pattern PcsLShiftA = LShiftA () ()
pattern PcsRShiftA = RShiftA () ()
pattern PcsRRShiftA = RRShiftA () ()
pattern PcsAndA = AndA () ()
pattern PcsXorA = XorA () ()
pattern PcsOrA = OrA () ()

pattern PcsNameLhs n = NameLhs () () n
pattern PcsFieldLhs fa = FieldLhs () () fa
pattern PcsArrayLhs ai = ArrayLhs () () ai

pattern PcsArrayIndex e1 e2 = ArrayIndex () () e1 e2

pattern PcsPrimaryFieldAccess e id = PrimaryFieldAccess () () e id
pattern PcsSuperFieldAccess id = SuperFieldAccess () () id
pattern PcsClassFieldAccess n id = ClassFieldAccess () () n id

pattern PcsMethodCallOrLockQuery n a = MethodCallOrLockQuery () () n a
pattern PcsPrimaryMethodCall e nwta id a = PrimaryMethodCall () () e nwta id a
pattern PcsSuperMethodCall nwta id a = SuperMethodCall () () nwta id a
pattern PcsClassMethodCall n nwta id a = ClassMethodCall () () n nwta id a
pattern PcsTypeMethodCall n nwta i a = TypeMethodCall () () n nwta i a

pattern PcsArrayInit vi = ArrayInit () () vi

pattern PcsVoidType = VoidType () ()
pattern PcsLockType = LockType () ()
pattern PcsType t = Type () () t

pattern PcsPrimType pt = PrimType () () pt
pattern PcsRefType rt = RefType () () rt

pattern PcsClassRefType ct = ClassRefType () () ct
pattern PcsTypeVariable id = TypeVariable () () id
pattern PcsArrayType t mp = ArrayType () () t mp

pattern PcsClassType n ta = ClassType () () n ta

pattern PcsWildcard mwb = Wildcard () () mwb
pattern PcsActualArg nwta = ActualArg () () nwta

pattern PcsActualName n = ActualName () () n
pattern PcsActualType rt = ActualType () () rt
pattern PcsActualExp e = ActualExp () () e
pattern PcsActualLockState l = ActualLockState () () l

pattern PcsExtendsBound rt = ExtendsBound () () rt
pattern PcsSuperBound rt = SuperBound () () rt

pattern PcsBooleanT = BooleanT () ()
pattern PcsByteT = ByteT () ()
pattern PcsShortT = ShortT () ()
pattern PcsIntT = IntT () ()
pattern PcsLongT = LongT () ()
pattern PcsCharT = CharT () ()
pattern PcsFloatT = FloatT () ()
pattern PcsDoubleT = DoubleT () ()
pattern PcsActorT = ActorT () ()
pattern PcsPolicyT = PolicyT () ()

pattern PcsActorParam rt id = ActorParam () () rt id
pattern PcsPolicyParam id = PolicyParam () () id
pattern PcsLockStateParam id = LockStateParam () () id

pattern PcsPolicyLit c = PolicyLit () () c
pattern PcsPolicyOf id = PolicyOf () () id
pattern PcsPolicyThis = PolicyThis () ()
pattern PcsPolicyTypeVar id = PolicyTypeVar () () id

pattern PcsLockProperties lc = LockProperties () () lc

pattern PcsClause cvd ch a = Clause () () cvd ch a

pattern PcsClauseVarDecl rt i = ClauseVarDecl () () rt i

pattern PcsClauseDeclHead cvd = ClauseDeclHead () () cvd
pattern PcsClauseVarHead a = ClauseVarHead () () a

pattern PcsLClause cvd a1 a2 = LClause () () cvd a1 a2
pattern PcsConstraintClause cvd a = ConstraintClause () () cvd a

pattern PcsActor an = Actor () () an
pattern PcsVar id = Var () () id

pattern PcsActorName n = ActorName () () n
pattern PcsActorTypeVar rt id = ActorTypeVar () () rt id

pattern PcsAtom n a = Atom () () n a

pattern PcsLock n an = Lock () () n an
pattern PcsLockVar id = LockVar () () id

pattern PcsIdent bs = Ident () () bs

type instance XSP () = ()
type instance XOp () = ()
