{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternSynonyms #-}

module Language.Java.Paragon.Decorations.PcsDecoration where
import Language.Java.Paragon.Decorations.DecorationTypes
import Language.Java.Paragon.SourcePos
import Language.Java.Paragon.TypesTTG
import Language.Java.Paragon.SyntaxTTG
import Data.Void

data PCS
type PcsPlaceHolder = ()

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
type instance XName                PCS = NoFieldExt

pattern PcsTypeParam id re = TypeParam () id re

pattern PcsCompilationUnit mpd id td = CompilationUnit () mpd id td

pattern PcsPackageDecl n = PackageDecl () n

pattern PcsSingleTypeImport n = SingleTypeImport () n
pattern PcsTypeImportOnDemand n = TypeImportOnDemand () n
pattern PcsSingleStaticImport n id = SingleStaticImport () n id
pattern PcsStaticImportOnDemand n = StaticImportOnDemand () n

pattern PcsClassTypeDecl cl = ClassTypeDecl () cl
pattern PcsInterfaceTypeDecl id = InterfaceTypeDecl () id

pattern PcsClassDecl mod id tp mct ct cb = ClassDecl () mod id tp mct ct cb
pattern PcsEnumDecl mod id ct eb = EnumDecl () mod id ct eb

pattern PcsClassBody d = ClassBody () d

pattern PcsEnumBody ec d = EnumBody () ec d

pattern PcsEnumConstant id arg mcb = EnumConstant () id arg mcb

pattern PcsInterfaceDecl mod id tp ct ib = InterfaceDecl () mod id tp ct ib

pattern PcsInterfaceBody md = InterfaceBody () md

pattern PcsMemberDecl md = MemberDecl () md
pattern PcsInitDecl b bl = InitDecl () b bl

pattern PcsFieldDecl mod t vd = FieldDecl () mod t vd
pattern PcsMethodDecl mod tp rt id fp es mb = MethodDecl () mod tp rt id fp es mb
pattern PcsConstructorDecl mod tp id fp es cb = ConstructorDecl () mod tp id fp es cb
pattern PcsMemberClassDecl cd = MemberClassDecl () cd
pattern PcsMemberInterfaceDecl id = MemberInterfaceDecl () id
pattern PcsLockDecl mod id rt mlp = LockDecl () mod id rt mlp

pattern PcsVarDecl vdi mvi = VarDecl () vdi mvi

pattern PcsVarId id = VarId () id
pattern PcsVarDeclArray vdi = VarDeclArray () vdi

pattern PcsInitExp ex = InitExp () ex
pattern PcsInitArray ai = InitArray () ai

pattern PcsFormalParam mod t b vdi = FormalParam () mod t b vdi

pattern PcsMethodBody mb = MethodBody () mb

pattern PcsConstructorBody meci bs = ConstructorBody () meci bs

pattern PcsThisInvoke nwta a = ThisInvoke () nwta a
pattern PcsSuperInvoke nwta a = SuperInvoke () nwta a
pattern PcsPrimarySuperInvoke e nwta a = PrimarySuperInvoke () e nwta a

pattern PcsPublic = Public ()
pattern PcsPrivate = Private ()
pattern PcsProtected = Protected ()
pattern PcsAbstract = Abstract ()
pattern PcsFinal = Final ()
pattern PcsStatic = Static ()
pattern PcsStrictFP = StrictFP ()
pattern PcsTransient = Transient ()
pattern PcsVolatile = Volatile ()
pattern PcsNative = Native ()

pattern PcsTypemethod = Typemethod ()
pattern PcsReflexive = Reflexive ()
pattern PcsTransitive = Transitive ()
pattern PcsSymmetric = Symmetric ()
pattern PcsReadonly = Readonly ()
pattern PcsNotnull = Notnull ()

pattern PcsReads p = Reads () p
pattern PcsWrites p = Writes () p
pattern PcsOpens l = Opens () l
pattern PcsCloses l = Closes () l
pattern PcsExpects l = Expects () l

pattern PcsBlock bs = Block () bs

pattern PcsBlockStmt st = BlockStmt () st
pattern PcsLocalClass cd = LocalClass () cd
pattern PcsLocalVars mod t vd = LocalVars () mod t vd
pattern PcsLocalLock mod id rt mlp = LocalLock () mod id rt mlp

pattern PcsStmtBlock b = StmtBlock () b
pattern PcsIfThen e s = IfThen () e s
pattern PcsIfThenElse e s1 s2 = IfThenElse () e s1 s2
pattern PcsWhile e s = While () e s
pattern PcsBasicFor mfi me mes st = BasicFor () mfi me mes st
pattern PcsEnhancedFor mod t id e st = EnhancedFor () mod t id e st
pattern PcsEmpty = Empty ()
pattern PcsExpStmt e = ExpStmt () e
pattern PcsAssert e me = Assert () e me
pattern PcsSwitch e sb = Switch () e sb
pattern PcsDo s e = Do () s e
pattern PcsBreak mid = Break () mid
pattern PcsContinue mid = Continue () mid
pattern PcsReturn me = Return () me
pattern PcsSynchronized e b = Synchronized () e b
pattern PcsThrow e = Throw () e
pattern PcsTry b c mb = Try () b c mb
pattern PcsLabeled id s = Labeled () id s
pattern PcsOpen l = Open () l
pattern PcsClose l = Close () l
pattern PcsOpenBlock l b = OpenBlock () l b
pattern PcsCloseBlock l b = CloseBlock () l b

pattern PcsCatch fp b = Catch () fp b

pattern PcsSwitchBlock sl bs = SwitchBlock () sl bs

pattern PcsSwitchCase e = SwitchCase () e
pattern PcsDefault = Default ()

pattern PcsForLocalVars mod t vd = ForLocalVars () mod t vd
pattern PcsForInitExps e = ForInitExps () e

pattern PcsExceptionSpec mod et = ExceptionSpec () mod et

pattern PcsLit l = Lit () l
pattern PcsClassLit mt = ClassLit () mt
pattern PcsThis = This ()
pattern PcsThisClass n = ThisClass () n
pattern PcsParen e = Paren () e
pattern PcsInstanceCreation ta ct a mcb = InstanceCreation () ta ct a mcb
pattern PcsQualInstanceCreation e ta id a mcb = QualInstanceCreation () e ta id a mcb
pattern PcsArrayCreate t emp mp = ArrayCreate () t emp mp
pattern PcsArrayCreateInit t mp ai = ArrayCreateInit () t mp ai
pattern PcsFieldAccess fa = FieldAccess () fa
pattern PcsMethodInv mi = MethodInv () mi
pattern PcsArrayAccess ai = ArrayAccess () ai
pattern PcsExpName n = ExpName () n
pattern PcsPostIncrement e = PostIncrement () e
pattern PcsPostDecrement e = PostDecrement () e
pattern PcsPreIncrement e = PreIncrement () e
pattern PcsPreDecrement e = PreDecrement () e
pattern PcsPrePlus e = PrePlus () e
pattern PcsPreMinus e = PreMinus () e
pattern PcsPreBitCompl e = PreBitCompl () e
pattern PcsPreNot e = PreNot () e
pattern PcsCast t e = Cast () t e
pattern PcsBinOp e1 o e2 = BinOp () e1 o e2
pattern PcsInstanceOf e rt = InstanceOf () e rt
pattern PcsCond e1 e2 e3 = Cond () e1 e2 e3
pattern PcsAssign l ao e = Assign () l ao e
pattern PcsPolicyExp pe = PolicyExp () pe
pattern PcsLockExp l = LockExp () l

pattern PcsInt i = Int () i
pattern PcsWord i = Word () i
pattern PcsFloat d = Float () d
pattern PcsDouble d = Double () d
pattern PcsBoolean b = Boolean () b
pattern PcsChar c = Char () c
pattern PcsString s = String () s
pattern PcsNull = Null ()

pattern PcsMult = Mult ()
pattern PcsDiv = Div ()
pattern PcsRem = Rem ()
pattern PcsAdd = Add ()
pattern PcsSub = Sub ()
pattern PcsLShift = LShift ()
pattern PcsRShift = RShift ()
pattern PcsRRShift = RRShift ()
pattern PcsLThan = LThan ()
pattern PcsGThan = GThan ()
pattern PcsLThanE = LThanE ()
pattern PcsGThanE = GThanE ()
pattern PcsEqual = Equal ()
pattern PcsNotEq = NotEq ()
pattern PcsAnd = And ()
pattern PcsOr = Or ()
pattern PcsXor = Xor ()
pattern PcsCAnd = CAnd ()
pattern PcsCOr = COr ()

pattern PcsEqualA = EqualA ()
pattern PcsMultA = MultA ()
pattern PcsDivA = DivA ()
pattern PcsRemA = RemA ()
pattern PcsAddA = AddA ()
pattern PcsSubA = SubA ()
pattern PcsLShiftA = LShiftA ()
pattern PcsRShiftA = RShiftA ()
pattern PcsRRShiftA = RRShiftA ()
pattern PcsAndA = AndA ()
pattern PcsXorA = XorA ()
pattern PcsOrA = OrA ()

pattern PcsNameLhs n = NameLhs () n
pattern PcsFieldLhs fa = FieldLhs () fa
pattern PcsArrayLhs ai = ArrayLhs () ai

pattern PcsArrayIndex e1 e2 = ArrayIndex () e1 e2

pattern PcsPrimaryFieldAccess e id = PrimaryFieldAccess () e id
pattern PcsSuperFieldAccess id = SuperFieldAccess () id
pattern PcsClassFieldAccess n id = ClassFieldAccess () n id

pattern PcsMethodCallOrLockQuery n a = MethodCallOrLockQuery () n a
pattern PcsPrimaryMethodCall e nwta id a = PrimaryMethodCall () e nwta id a
pattern PcsSuperMethodCall nwta id a = SuperMethodCall () nwta id a
pattern PcsClassMethodCall n nwta id a = ClassMethodCall () n nwta id a
pattern PcsTypeMethodCall n nwta i a = TypeMethodCall () n nwta i a

pattern PcsArrayInit vi = ArrayInit () vi

pattern PcsVoidType = VoidType ()
pattern PcsLockType = LockType ()
pattern PcsType t = Type () t

pattern PcsPrimType pt = PrimType () pt
pattern PcsRefType rt = RefType () rt

pattern PcsClassRefType ct = ClassRefType () ct
pattern PcsTypeVariable id = TypeVariable () id
pattern PcsArrayType t mp = ArrayType () t mp

pattern PcsClassType n ta = ClassType () n ta

pattern PcsWildcard mwb = Wildcard () mwb
pattern PcsActualArg nwta = ActualArg () nwta

pattern PcsActualName n = ActualName () n
pattern PcsActualType rt = ActualType () rt
pattern PcsActualExp e = ActualExp () e
pattern PcsActualLockState l = ActualLockState () l

pattern PcsExtendsBound rt = ExtendsBound () rt
pattern PcsSuperBound rt = SuperBound () rt

pattern PcsBooleanT = BooleanT ()
pattern PcsByteT = ByteT ()
pattern PcsShortT = ShortT ()
pattern PcsIntT = IntT ()
pattern PcsLongT = LongT ()
pattern PcsCharT = CharT ()
pattern PcsFloatT = FloatT ()
pattern PcsDoubleT = DoubleT ()
pattern PcsActorT = ActorT ()
pattern PcsPolicyT = PolicyT ()

pattern PcsActorParam rt id = ActorParam () rt id
pattern PcsPolicyParam id = PolicyParam () id
pattern PcsLockStateParam id = LockStateParam () id

pattern PcsPolicyLit c = PolicyLit () c
pattern PcsPolicyOf id = PolicyOf () id
pattern PcsPolicyThis = PolicyThis ()
pattern PcsPolicyTypeVar id = PolicyTypeVar () id

pattern PcsLockProperties lc = LockProperties () lc

pattern PcsClause cvd ch a = Clause () cvd ch a

pattern PcsClauseVarDecl rt i = ClauseVarDecl () rt i

pattern PcsClauseDeclHead cvd = ClauseDeclHead () cvd
pattern PcsClauseVarHead a = ClauseVarHead () a

pattern PcsLClause cvd a1 a2 = LClause () cvd a1 a2
pattern PcsConstraintClause cvd a = ConstraintClause () cvd a

pattern PcsActor an = Actor () an
pattern PcsVar id = Var () id

pattern PcsActorName n = ActorName () n
pattern PcsActorTypeVar rt id = ActorTypeVar () rt id

pattern PcsAtom n a = Atom () n a

pattern PcsLock n an = Lock () n an
pattern PcsLockVar id = LockVar () id

pattern PcsIdent bs = Ident () bs

pattern PcsName nt mn id = Name () nt mn id