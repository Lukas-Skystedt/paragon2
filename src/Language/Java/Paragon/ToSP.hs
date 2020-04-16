{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Language.Java.Paragon.ToSP where

import Language.Java.Paragon.Syntax
import Language.Java.Paragon.TypeCheck.Types
import Language.Java.Paragon.Decorations.PaDecoration
import Language.Java.Paragon.SourcePos (defaultPos)

import Prelude hiding (exp)

class ToSP ast d where
  toSP :: ast d -> ast PA


instance ToSP PackageDecl TC where
  toSP (PackageDecl _ name) = PackageDecl defaultPos (toSP name)

instance ToSP ImportDecl TC where
  toSP (SingleTypeImport     _ name) =
    SingleTypeImport     defaultPos (toSP name)
  toSP (TypeImportOnDemand   _ name) =
    TypeImportOnDemand   defaultPos (toSP name)
  toSP (SingleStaticImport   _ name ident) =
    SingleStaticImport   defaultPos (toSP name) (toSP ident)
  toSP (StaticImportOnDemand _ name) =
    StaticImportOnDemand defaultPos (toSP name)

instance ToSP ClassDecl TC where
  toSP (ClassDecl _ mods ident tPars cType cTypes cBody) =
    ClassDecl defaultPos (toSP <$> mods) (toSP ident) (toSP <$> tPars)
      (toSP <$> cType) (toSP <$> cTypes) (toSP cBody)

--   toSP (EnumDecl _ mods ident cTypes enumB) =
--     EnumDecl defaultPos (toSP <$> mods) (toSP ident) (toSP <$> cTypes)
--       (toSP enumB)

instance ToSP ClassBody TC where
  toSP (ClassBody _ decls) = ClassBody defaultPos (toSP <$> decls)

-- instance ToSP EnumBody TC where
--   toSP (EnumBody _ eConsts decls) =
--     EnumBody defaultPos (toSP <$> eConsts) (toSP <$> decls)

-- instance ToSP EnumConstant TC where
--   toSP (EnumConstant _ ident args cBody) =
--     EnumConstant defaultPos (toSP ident) (toSP <$> args) (toSP <$> cBody)

instance ToSP InterfaceDecl TC where
  toSP (InterfaceDecl _ mods ident tParams cTypes ifaceB) =
    InterfaceDecl defaultPos (toSP <$> mods) (toSP ident) (toSP <$> tParams)
      (toSP <$> cTypes) (toSP ifaceB)

instance ToSP InterfaceBody TC where
  toSP (InterfaceBody _ mDecls) = InterfaceBody defaultPos (toSP <$> mDecls)

instance ToSP Decl TC where
  toSP (MemberDecl _ mDecl) = MemberDecl defaultPos (toSP mDecl)
  toSP (InitDecl _ b block) = InitDecl defaultPos b (toSP block)

instance ToSP MemberDecl TC where
  toSP (FieldDecl _ mods typ vDecl) =
    FieldDecl defaultPos (toSP <$> mods) (toSP typ) (toSP <$> vDecl)
  toSP (MethodDecl _ mods tPars rTyp ident fPars exSpecs mBody) =
    MethodDecl defaultPos (toSP <$> mods) (toSP <$> tPars) (toSP rTyp)
      (toSP ident) (toSP <$> fPars) (toSP <$> exSpecs) (toSP mBody)
  toSP (ConstructorDecl _ mods tPars ident fPars exSpecs cBody) =
    ConstructorDecl defaultPos (toSP <$> mods) (toSP <$> tPars) (toSP ident)
      (toSP <$> fPars) (toSP <$> exSpecs) (toSP cBody)
  toSP (MemberClassDecl _ cDecl) = MemberClassDecl defaultPos (toSP cDecl)
  toSP (MemberInterfaceDecl _ iFaceDecl) =
    MemberInterfaceDecl defaultPos (toSP iFaceDecl)

-- -- Paragon specific
--     | LockDecl (XMemberDecl x) [Modifier x]  (Ident x) [RefType x] (Maybe (LockProperties x))


instance ToSP VarDecl TC where
  toSP (VarDecl _ vDeclId vInit) =
    VarDecl defaultPos (toSP vDeclId) (toSP <$> vInit)

instance ToSP VarDeclId TC where
  toSP (VarId _ ident)          = VarId defaultPos (toSP ident)
  toSP (VarDeclArray _ vDeclId) = VarDeclArray defaultPos (toSP vDeclId)

instance ToSP VarInit TC where
  toSP (InitExp _ exp)      = InitExp defaultPos (toSP exp)
  toSP (InitArray _ arInit) = error "toSP: case InitArray not implemented" --InitArray defaultPos (toSP arInit)

instance ToSP FormalParam TC where
  toSP (FormalParam _ mods typ b vDeclId) =
    FormalParam defaultPos (toSP <$> mods) (toSP typ) b (toSP vDeclId)

instance ToSP MethodBody TC where
  toSP (MethodBody _ block) = MethodBody defaultPos (toSP <$> block)

instance ToSP ConstructorBody TC where
  toSP (ConstructorBody _ eCInv blockStms) =
    ConstructorBody defaultPos (toSP <$> eCInv) (toSP <$> blockStms)


instance ToSP ExplConstrInv TC where
  toSP (ThisInvoke _ nwta arg) = ThisInvoke defaultPos (toSP <$> nwta) (toSP <$> arg)
  toSP (SuperInvoke _ nwta arg) =
    SuperInvoke defaultPos (toSP <$> nwta) (toSP <$> arg)
  toSP (PrimarySuperInvoke _ exp nwta arg) =
    PrimarySuperInvoke defaultPos (toSP exp) (toSP <$> nwta) (toSP <$> arg)

instance ToSP Modifier TC where
  toSP (Public    _) = Public defaultPos
  toSP (Private   _) = Private defaultPos
  toSP (Protected _) = Protected defaultPos
  toSP (Abstract  _) = Abstract defaultPos
  toSP (Final     _) = Final defaultPos
  toSP (Static    _) = Static defaultPos
  toSP (StrictFP  _) = StrictFP defaultPos
  toSP (Transient _) = Transient defaultPos
  toSP (Volatile  _) = Volatile defaultPos
  toSP (Native    _) = Native defaultPos
-- Paragon specific
  toSP (Typemethod _) = Typemethod defaultPos
  toSP (Reflexive  _) = Reflexive defaultPos
  toSP (Transitive _) = Transitive defaultPos
  toSP (Symmetric  _) = Symmetric defaultPos
  toSP (Readonly   _) = Readonly defaultPos
  toSP (Notnull    _) = Notnull defaultPos

  toSP (Reads   _ p ) = Reads   defaultPos (toSP p) 
  toSP (Writes  _ p ) = Writes  defaultPos (toSP p) 
  toSP (Opens   _ ls) = Opens   defaultPos (toSP <$> ls)
  toSP (Closes  _ ls) = Closes  defaultPos (toSP <$> ls)
  toSP (Expects _ ls) = Expects defaultPos (toSP <$> ls) 

instance ToSP Block TC where
  toSP (Block _ bStms) = Block defaultPos (toSP <$> bStms)

instance ToSP BlockStmt TC where
  toSP (BlockStmt _ stm)             = BlockStmt defaultPos (toSP stm)
  toSP (LocalClass _ cDecl)          = LocalClass defaultPos (toSP cDecl)
  toSP (LocalVars _ mods typ vDecls) =
    LocalVars defaultPos (toSP <$> mods) (toSP typ) (toSP <$> vDecls)

-- -- -- Paragon specific
-- --     | LocalLock (XBlockStm x) [Modifier x] (Ident x) [RefType x]
-- --       (Maybe (LockProperties x))

instance ToSP Stmt TC where
  toSP (StmtBlock _ block) = StmtBlock defaultPos (toSP block)
  toSP (IfThen _ exp stm) =
    IfThen defaultPos (toSP exp) (toSP stm)
  toSP (IfThenElse _ exp s1 s2) =
    IfThenElse defaultPos (toSP exp) (toSP s1) (toSP s2)
  toSP (While _ exp stm) = While defaultPos (toSP exp) (toSP stm)
  toSP (BasicFor _ mfInit mExp mExps stm) =
    BasicFor defaultPos (toSP <$> mfInit) (toSP <$> mExp)
      (fmap toSP <$> mExps) (toSP stm)

  toSP (EnhancedFor _ mods typ ident exp stm) =
    EnhancedFor defaultPos (toSP <$> mods) (toSP typ) (toSP ident)
      (toSP exp) (toSP stm)

  toSP (Empty _)                  = Empty defaultPos
  toSP (ExpStmt _ exp)            = ExpStmt defaultPos (toSP exp)
  toSP (Assert _ exp mExp)        =
    Assert defaultPos (toSP exp) (toSP <$> mExp)
  toSP (Switch _ exp sBlocks)     =
    Switch defaultPos (toSP exp) (toSP <$> sBlocks)
  toSP (Do _ stm exp)             = Do defaultPos (toSP stm) (toSP exp)
  toSP (Break _ ident)            = Break defaultPos (toSP <$> ident)
  toSP (Continue _ ident)         = Continue defaultPos (toSP <$> ident)
  toSP (Return _ exp)             = Return defaultPos (toSP <$> exp)
  toSP (Synchronized _ exp block) =
    Synchronized defaultPos (toSP exp) (toSP block)
  toSP (Throw _ exp)              = Throw defaultPos (toSP exp)
  toSP (Try _ block cs mBlock)    =
    Try defaultPos (toSP block) (toSP <$> cs) (toSP <$> mBlock)
  toSP (Labeled _ ident stmt)     =
    Labeled defaultPos (toSP ident) (toSP stmt)

-- -- -- Paragon specific
-- --     | Open  (XStm x) (Lock x)
-- --     | Close (XStm x) (Lock x)
-- --     | OpenBlock  (XStm x) (Lock x) (Block x)
-- --     | CloseBlock (XStm x) (Lock x) (Block x)



instance ToSP Catch TC where
  toSP (Catch _ fParam block) =
    Catch defaultPos (toSP fParam) (toSP block)

instance ToSP SwitchBlock TC where
  toSP (SwitchBlock _ sLabel bStms) =
    SwitchBlock defaultPos (toSP sLabel) (toSP <$> bStms)

instance ToSP SwitchLabel TC where
  toSP (SwitchCase _ exp) = SwitchCase defaultPos (toSP exp)
  toSP (Default _) = Default defaultPos

instance ToSP ForInit TC where
  toSP (ForLocalVars _ mods typ vDecls) =
    ForLocalVars defaultPos (toSP <$> mods) (toSP typ) (toSP <$> vDecls)
  toSP (ForInitExps _ exps) = ForInitExps defaultPos (toSP <$> exps)


instance ToSP ExceptionSpec TC where
  toSP (ExceptionSpec _ mods excTyp) =
    ExceptionSpec defaultPos (toSP <$> mods) (toSP excTyp)

instance ToSP Exp TC where
  toSP (Lit _ lit)        = Lit defaultPos (toSP lit)
  toSP (ClassLit _ typ)   = ClassLit defaultPos (toSP <$> typ)
  toSP (This _)           = This defaultPos
  toSP (ThisClass _ name) = ThisClass defaultPos (toSP name)
  toSP (Paren _ exp)      = Paren defaultPos (toSP exp)
  toSP (InstanceCreation _ tArgs cType args cBody) =
    InstanceCreation defaultPos (toSP <$> tArgs) (toSP cType) (toSP <$> args)
      (toSP <$> cBody)

  toSP (QualInstanceCreation _ exp tArgs ident args cBody) =
    QualInstanceCreation defaultPos (toSP exp) (toSP <$> tArgs) (toSP ident)
      (toSP <$> args) (toSP <$> cBody)

  toSP (ArrayCreate _ typ expnPols mPols) =
    ArrayCreate defaultPos (toSP typ)
      (map (\(a,b) -> (toSP a,toSP <$> b)) expnPols)
      (map (toSP <$>) mPols)

  toSP (ArrayCreateInit _ t pols aInit) =
    ArrayCreateInit defaultPos (toSP t) (map (toSP <$>) pols)
      (toSP aInit)

  toSP (FieldAccess _ fAccess) = FieldAccess defaultPos (toSP fAccess)
  toSP (MethodInv _ mInvoke)   = MethodInv defaultPos (toSP mInvoke)
  toSP (ArrayAccess _ aIndex)  = ArrayAccess defaultPos (toSP aIndex)
  toSP (ExpName _ name)        = ExpName defaultPos (toSP name)
  toSP (PostIncrement _ exp)   = PostIncrement defaultPos (toSP exp)
  toSP (PostDecrement _ exp)   = PostDecrement defaultPos (toSP exp)
  toSP (PreIncrement  _ exp)   = PreIncrement defaultPos (toSP exp)
  toSP (PreDecrement  _ exp)   = PreDecrement defaultPos (toSP exp)
  toSP (PrePlus  _ exp)        = PrePlus defaultPos (toSP exp)
  toSP (PreMinus _ exp)        = PreMinus defaultPos (toSP exp)
  toSP (PreBitCompl _ exp)     = PreBitCompl defaultPos (toSP exp)
  toSP (PreNot  _ exp)         = PreNot defaultPos (toSP exp)
  toSP (Cast _ typ exp)        = Cast defaultPos (toSP typ) (toSP exp)
  toSP (BinOp _ e1 op e2)      =
    BinOp defaultPos (toSP e1) (toSP op) (toSP e2)
  toSP (InstanceOf _ exp refT) =
    InstanceOf defaultPos (toSP exp) (toSP refT)
  toSP (Cond _ e1 e2 e3)       =
    Cond defaultPos (toSP e1) (toSP e2) (toSP e3)
  toSP (Assign _ lhs aOp exp)  =
    Assign defaultPos (toSP lhs) (toSP aOp) (toSP exp)

-- -- Paragon specific
  toSP (PolicyExp _ pe) = PolicyExp defaultPos (toSP pe)
  toSP (LockExp _ l) = LockExp defaultPos (toSP l)

-- -- Quasi-quotation
--     | AntiQExp (XExp x) String

instance ToSP PolicyExp TC where
  toSP (PolicyLit     _ cs) = PolicyLit defaultPos (toSP <$> cs)
  toSP (PolicyOf      _ i) = PolicyOf defaultPos (toSP i)
  toSP (PolicyThis    _) = PolicyThis defaultPos
  toSP (PolicyTypeVar _ i) = PolicyTypeVar defaultPos (toSP i)

instance ToSP Clause TC where
  toSP (Clause _ cvds ch a) = Clause defaultPos (toSP <$> cvds) (toSP ch) (toSP <$> a)

instance ToSP Literal TC where
  toSP (Int     _ i) = Int     defaultPos i
  toSP (Word    _ i) = Word    defaultPos i
  toSP (Float   _ d) = Float   defaultPos d
  toSP (Double  _ d) = Double  defaultPos d
  toSP (Boolean _ b) = Boolean defaultPos b
  toSP (Char    _ c) = Char    defaultPos c
  toSP (String  _ s) = String  defaultPos s
  toSP (Null    _  ) = Null    defaultPos

instance ToSP Op TC where
  toSP (Mult _)    = Mult defaultPos 
  toSP (Div _)     = Div defaultPos 
  toSP (Rem _)     = Rem defaultPos
  toSP (Add _)     = Add defaultPos
  toSP (Sub _)     = Sub defaultPos
  toSP (LShift _)  = LShift defaultPos
  toSP (RShift _)  = RShift defaultPos
  toSP (RRShift _) = RRShift defaultPos
  toSP (LThan _)   = LThan defaultPos
  toSP (GThan _)   = GThan defaultPos
  toSP (LThanE _)  = LThanE defaultPos
  toSP (GThanE _)  = GThanE defaultPos
  toSP (Equal _)   = Equal defaultPos
  toSP (NotEq _)   = NotEq defaultPos
  toSP (And _)     = And defaultPos
  toSP (Or _)      = Or defaultPos
  toSP (Xor _)     = Xor defaultPos
  toSP (CAnd _)    = CAnd defaultPos
  toSP (COr _)     = COr defaultPos

instance ToSP AssignOp TC where
  toSP (EqualA _)   = EqualA defaultPos
  toSP (MultA _)    = MultA defaultPos 
  toSP (DivA _)     = DivA defaultPos 
  toSP (RemA _)     = RemA defaultPos
  toSP (AddA _)     = AddA defaultPos
  toSP (SubA _)     = SubA defaultPos
  toSP (LShiftA _)  = LShiftA defaultPos
  toSP (RShiftA _)  = RShiftA defaultPos
  toSP (RRShiftA _) = RRShiftA defaultPos
  toSP (AndA _)     = AndA defaultPos
  toSP (XorA _)     = XorA defaultPos
  toSP (OrA _)      = OrA defaultPos

instance ToSP Lhs TC where
  toSP (NameLhs _ name)     = NameLhs defaultPos (toSP name)
  toSP (FieldLhs _ fAccess) = FieldLhs defaultPos (toSP fAccess)
  toSP (ArrayLhs _ aIndex)  = ArrayLhs defaultPos (toSP aIndex)


instance ToSP ArrayIndex TC where
 toSP (ArrayIndex _ e1 e2) = ArrayIndex defaultPos (toSP e1) (toSP e2)

instance ToSP FieldAccess TC where
  toSP (PrimaryFieldAccess _ exp ident) =
    PrimaryFieldAccess defaultPos (toSP exp) (toSP ident)
  toSP (SuperFieldAccess   _ ident) = SuperFieldAccess defaultPos (toSP ident)
  toSP (ClassFieldAccess   _ name ident) =
    ClassFieldAccess   defaultPos (toSP name) (toSP ident)

instance ToSP MethodInvocation TC where
  toSP (MethodCallOrLockQuery _ name args) =
    MethodCallOrLockQuery defaultPos (toSP name) (toSP <$> args)
  toSP (PrimaryMethodCall _ exp nwtargs ident args) =
    PrimaryMethodCall defaultPos (toSP exp) (toSP <$> nwtargs) (toSP ident)
      (toSP <$> args)
  toSP (SuperMethodCall _ nwtargs ident args) =
    SuperMethodCall defaultPos (toSP <$> nwtargs) (toSP ident) (toSP <$> args)
  toSP (ClassMethodCall _ name nwtargs ident args) =
    ClassMethodCall defaultPos (toSP name) (toSP <$> nwtargs) (toSP ident)
      (toSP <$> args)
  toSP (TypeMethodCall _ name nwtargs ident args) =
    TypeMethodCall defaultPos (toSP name) (toSP <$> nwtargs) (toSP ident)
      (toSP <$> args)


instance ToSP ArrayInit TC where
  toSP (ArrayInit _ vInits) = ArrayInit defaultPos (toSP <$> vInits)


instance ToSP ReturnType TC where
  toSP (VoidType _) = VoidType defaultPos
  toSP (LockType _) = LockType defaultPos
  toSP (Type _ typ) = Type defaultPos (toSP typ)

instance ToSP Type TC where
  toSP (PrimType  _ pType) = PrimType  defaultPos (toSP pType)
  toSP (RefType   _ rType) = RefType   defaultPos (toSP rType)
  toSP (AntiQType _ s)     = error "AntiQType should never occur in TypeCheck phase."

instance ToSP RefType TC where
  toSP (ClassRefType _ cType) = ClassRefType defaultPos (toSP cType)
  toSP (TypeVariable _ ident) = TypeVariable defaultPos (toSP ident)
  toSP (TcArrayType typ pol)  = error "toSP: case TcArrayType not implemented"

instance ToSP ClassType TC where
  toSP (ClassType _ name tArgs) =
    ClassType defaultPos (toSP name) (toSP <$> tArgs)

instance ToSP TypeArgument TC where
  toSP (TcActualType rt     )  = error "toSP: case TcActualType not implemented"
  toSP (TcActualPolicy ap   )  = error "toSP: case TcActualPolicy not implemented"
  toSP (TcActualActor taid  )  = error "toSP: case TcActualActor not implemented"
  toSP (TcActualLockState ls)  = error "toSP: case TcActualLockState not implemented"

instance ToSP NonWildTypeArgument TC where
  toSP (ActualName _ name)       = ActualName defaultPos (toSP name)
  toSP (ActualType _ rType)      = ActualType defaultPos (toSP rType)
  toSP (ActualExp _ eType)       = ActualExp defaultPos (toSP eType)
  toSP (ActualLockState _ locks) = ActualLockState defaultPos (toSP <$> locks)

-- instance ToSP WildcardBound TC where
--   toSP (ExtendsBound _ rType) = ExtendsBound defaultPos (toSP rType)
--   toSP (SuperBound _ rType)   = SuperBound defaultPos (toSP rType)

instance ToSP PrimType TC where
  toSP (BooleanT _) = BooleanT defaultPos
  toSP (ByteT    _) = ByteT    defaultPos
  toSP (ShortT   _) = ShortT   defaultPos
  toSP (IntT     _) = IntT     defaultPos
  toSP (LongT    _) = LongT    defaultPos
  toSP (CharT    _) = CharT    defaultPos
  toSP (FloatT   _) = FloatT   defaultPos
  toSP (DoubleT  _) = DoubleT  defaultPos
--Paragon
  toSP (ActorT   _) = ActorT   defaultPos
  toSP (PolicyT  _) = PolicyT  defaultPos

instance ToSP TypeParam TC where
  toSP (TypeParam _ ident refTypes) =
    TypeParam defaultPos (toSP ident) (toSP <$> refTypes)
-- -- Paragon specific
--                  | ActorParam    (XTypeParam x) (RefType x) (Ident x)
--                  | PolicyParam   (XTypeParam x) (Ident x)
--                  | LockStateParam(XTypeParam x) (Ident x)

instance ToSP ClauseVarDecl TC where
  toSP  (ClauseVarDecl _ rType ident) =
    ClauseVarDecl defaultPos (toSP rType) (toSP ident)

instance ToSP ClauseHead TC where
  toSP (ClauseDeclHead _ cVDecl) = ClauseDeclHead defaultPos (toSP cVDecl)
  toSP (ClauseVarHead _ actor)   = ClauseVarHead defaultPos (toSP actor)

instance ToSP Actor TC where
  toSP (Actor _ aNames) = Actor defaultPos (toSP aNames)
  toSP (Var   _ ident)  = Var defaultPos (toSP ident)

instance ToSP ActorName TC where
  toSP (ActorName _ name) = ActorName defaultPos (toSP name)
  toSP (ActorTypeVar _ rType ident) =
    ActorTypeVar defaultPos (toSP rType) (toSP ident)

instance ToSP Atom TC where
  toSP (Atom _ name acts) = Atom defaultPos (toSP name) (toSP <$> acts)

instance ToSP Lock TC where
  toSP (Lock _ name aNames) = Lock defaultPos (toSP name) (toSP <$> aNames)
  toSP (LockVar _ ident)    = LockVar defaultPos (toSP ident)

instance ToSP Ident TC where
  toSP (Ident pos bString) = Ident pos bString
  toSP (AntiQIdent pos s)  = AntiQIdent pos s

instance ToSP Name TC where
  toSP (Name sp nType mName ident) =
    Name sp nType (toSP <$> mName) (toSP ident)
  toSP (AntiQName sp s) = AntiQName sp s
