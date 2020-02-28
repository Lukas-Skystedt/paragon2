{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternSynonyms #-}

module Language.Java.Paragon.Decorations.PaDecoration where
import Language.Java.Paragon.Decorations.DecorationTypes
import Language.Java.Paragon.SourcePos
import Language.Java.Paragon.TypesTTG
import Language.Java.Paragon.SyntaxTTG
import Data.Void
import Data.Data

data Pa deriving Data

-- This is a hack.
type instance XSP () = ()
type instance XOp () = ()

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
