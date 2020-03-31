{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Class VoidAnn with instances used to annotate the AST with ()
-- This module is only used for the purpose of deleting annotations.

-- This created to swap the old function `voidAnn` with the new function `removeAnn`.
-- The old function `voidAnn` may still be used in some places and if this
-- is the case it may be switched to `removeAnn` by making sure that it
-- exists an instance of VoidAnn for the data type used by `voidAnn`.

-- TODO Its possible that the instances also need to include
-- the paragon specific features (currently commented out).

module Language.Java.Paragon.RemoveAnnsTTG where
import Language.Java.Paragon.SyntaxTTG
import Language.Java.Paragon.Decorations.PaDecoration
import Prelude hiding (exp)

-- | VoidAnn provides a function for removing annotations.
-- Example usage is, Constructor (TypeFam x) -> Constructor ()
class VoidAnn a where
  removeAnn :: a b -> a ()

-- | Alias for removing annotations inside lists
fRem :: Functor f => VoidAnn a => f (a x) -> f (a ())
fRem = fRem

-- Instances for the data types that currently needs to remove
-- annotations.

instance VoidAnn PackageDecl where
  removeAnn (PackageDecl _ name) = PackageDecl () (removeAnn name)

instance VoidAnn ImportDecl where
  removeAnn (SingleTypeImport     _ name) =
    SingleTypeImport     () (removeAnn name)
  removeAnn (TypeImportOnDemand   _ name) =
    TypeImportOnDemand   () (removeAnn name)
  removeAnn (SingleStaticImport   _ name ident) =
    SingleStaticImport   () (removeAnn name) (removeAnn ident)
  removeAnn (StaticImportOnDemand _ name) =
    StaticImportOnDemand () (removeAnn name)

instance VoidAnn ClassDecl where
  removeAnn (ClassDecl _ mods ident tPars cType cTypes cBody) =
    ClassDecl () (fRem mods) (removeAnn ident) (fRem tPars)
      (fRem cType) (fRem cTypes) (removeAnn cBody)

  removeAnn (EnumDecl _ mods ident cTypes enumB) =
    EnumDecl () (fRem mods) (removeAnn ident) (fRem cTypes)
      (removeAnn enumB)

instance VoidAnn ClassBody where
  removeAnn (ClassBody _ decls) = ClassBody () (fRem decls)

instance VoidAnn EnumBody where
  removeAnn (EnumBody _ eConsts decls) =
    EnumBody () (fRem eConsts) (fRem decls)

instance VoidAnn EnumConstant where
  removeAnn (EnumConstant _ ident args cBody) =
    EnumConstant () (removeAnn ident) (fRem args) (fRem cBody)

instance VoidAnn InterfaceDecl where
  removeAnn (InterfaceDecl _ mods ident tParams cTypes ifaceB) =
    InterfaceDecl () (fRem mods) (removeAnn ident) (fRem tParams)
      (fRem cTypes) (removeAnn ifaceB)

instance VoidAnn InterfaceBody where
  removeAnn (InterfaceBody _ mDecls) = InterfaceBody () (fRem mDecls)

instance VoidAnn Decl where
  removeAnn (MemberDecl _ mDecl) = MemberDecl () (removeAnn mDecl)
  removeAnn (InitDecl _ b block) = InitDecl () b (removeAnn block)

instance VoidAnn MemberDecl where
  removeAnn (FieldDecl _ mods typ vDecl) =
    FieldDecl () (fRem mods) (removeAnn typ) (fRem vDecl)
  removeAnn (MethodDecl _ mods tPars rTyp ident fPars exSpecs mBody) =
    MethodDecl () (fRem mods) (fRem tPars) (removeAnn rTyp)
      (removeAnn ident) (fRem fPars) (fRem exSpecs) (removeAnn mBody)
  removeAnn (ConstructorDecl _ mods tPars ident fPars exSpecs cBody) =
    ConstructorDecl () (fRem mods) (fRem tPars) (removeAnn ident)
      (fRem fPars) (fRem exSpecs) (removeAnn cBody)
  removeAnn (MemberClassDecl _ cDecl) = MemberClassDecl () (removeAnn cDecl)
  removeAnn (MemberInterfaceDecl _ iFaceDecl) =
    MemberInterfaceDecl () (removeAnn iFaceDecl)

-- -- Paragon specific
--     | LockDecl (XMemberDecl x) [Modifier x]  (Ident x) [RefType x] (Maybe (LockProperties x))


instance VoidAnn VarDecl where
  removeAnn (VarDecl _ vDeclId vInit) =
    VarDecl () (removeAnn vDeclId) (fRem vInit)

instance VoidAnn VarDeclId where
  removeAnn (VarId _ ident)          = VarId () (removeAnn ident)
  removeAnn (VarDeclArray _ vDeclId) = VarDeclArray () (removeAnn vDeclId)

instance VoidAnn VarInit where
  removeAnn (InitExp _ exp)      = InitExp () (removeAnn exp)
  removeAnn (InitArray _ arInit) = InitArray () (removeAnn arInit)

instance VoidAnn FormalParam where
  removeAnn (FormalParam _ mods typ b vDeclId) =
    FormalParam () (fRem mods) (removeAnn typ) b (removeAnn vDeclId)

instance VoidAnn MethodBody where
  removeAnn (MethodBody _ block) = MethodBody () (fRem block)

instance VoidAnn ConstructorBody where
  removeAnn (ConstructorBody _ eCInv blockStms) =
    ConstructorBody () (fRem eCInv) (fRem blockStms)


instance VoidAnn ExplConstrInv where
  removeAnn (ThisInvoke _ nwta arg) = ThisInvoke () (fRem nwta) (fRem arg)
  removeAnn (SuperInvoke _ nwta arg) =
    SuperInvoke () (fRem nwta) (fRem arg)
  removeAnn (PrimarySuperInvoke _ exp nwta arg) =
    PrimarySuperInvoke () (removeAnn exp) (fRem nwta) (fRem arg)

instance VoidAnn Modifier where
  removeAnn (Public    _) = Public ()
  removeAnn (Private   _) = Private ()
  removeAnn (Protected _) = Protected ()
  removeAnn (Abstract  _) = Abstract ()
  removeAnn (Final     _) = Final ()
  removeAnn (Static    _) = Static ()
  removeAnn (StrictFP  _) = StrictFP ()
  removeAnn (Transient _) = Transient ()
  removeAnn (Volatile  _) = Volatile ()
  removeAnn (Native    _) = Native ()

-- Paragon specific
--   removeAnn (Typemethod _) = Typemethod ()
--   removeAnn (Reflexive  _) = Reflexive ()
--   removeAnn (Transitive _) = Transitive ()
--   removeAnn (Symmetric  _) = Symmetric ()
--   removeAnn (Readonly   _) = Readonly ()
--   removeAnn (Notnull    _) = Notnull ()

--  | Reads   (XMod x) (Policy x)
--  | Writes  (XMod x) (Policy x)
--  | Opens   (XMod x) [Lock x]
--  | Closes  (XMod x) [Lock x]
--  | Expects (XMod x) [Lock x]

instance VoidAnn Block where
  removeAnn (Block _ bStms) = Block () (fRem bStms)

instance VoidAnn BlockStmt where
  removeAnn (BlockStmt _ stm)             = BlockStmt () (removeAnn stm)
  removeAnn (LocalClass _ cDecl)          = LocalClass () (removeAnn cDecl)
  removeAnn (LocalVars _ mods typ vDecls) =
    LocalVars () (fRem mods) (removeAnn typ) (fRem vDecls)

-- -- Paragon specific
--     | LocalLock (XBlockStm x) [Modifier x] (Ident x) [RefType x]
--       (Maybe (LockProperties x))

instance VoidAnn Stmt where
  removeAnn (StmtBlock _ block) = StmtBlock () (removeAnn block)
  removeAnn (IfThen _ exp stm) =
    IfThen () (removeAnn exp) (removeAnn stm)
  removeAnn (IfThenElse _ exp s1 s2) =
    IfThenElse () (removeAnn exp) (removeAnn s1) (removeAnn s2)
  removeAnn (While _ exp stm) = While () (removeAnn exp) (removeAnn stm)
  removeAnn (BasicFor _ mfInit mExp mExps stm) =
    BasicFor () (fRem mfInit) (fRem mExp)
      (fmap fRem mExps) (removeAnn stm)

  removeAnn (EnhancedFor _ mods typ ident exp stm) =
    EnhancedFor () (fRem mods) (removeAnn typ) (removeAnn ident)
      (removeAnn exp) (removeAnn stm)

  removeAnn (Empty _)                  = Empty ()
  removeAnn (ExpStmt _ exp)            = ExpStmt () (removeAnn exp)
  removeAnn (Assert _ exp mExp)        =
    Assert () (removeAnn exp) (fRem mExp)
  removeAnn (Switch _ exp sBlocks)     =
    Switch () (removeAnn exp) (fRem sBlocks)
  removeAnn (Do _ stm exp)             = Do () (removeAnn stm) (removeAnn exp)
  removeAnn (Break _ ident)            = Break () (fRem ident)
  removeAnn (Continue _ ident)         = Continue () (fRem ident)
  removeAnn (Return _ exp)             = Return () (fRem exp)
  removeAnn (Synchronized _ exp block) =
    Synchronized () (removeAnn exp) (removeAnn block)
  removeAnn (Throw _ exp)              = Throw () (removeAnn exp)
  removeAnn (Try _ block cs mBlock)    =
    Try () (removeAnn block) (fRem cs) (fRem mBlock)
  removeAnn (Labeled _ ident stmt)     =
    Labeled () (removeAnn ident) (removeAnn stmt)

-- -- Paragon specific
--     | Open  (XStm x) (Lock x)
--     | Close (XStm x) (Lock x)
--     | OpenBlock  (XStm x) (Lock x) (Block x)
--     | CloseBlock (XStm x) (Lock x) (Block x)



instance VoidAnn Catch where
  removeAnn (Catch _ fParam block) =
    Catch () (removeAnn fParam) (removeAnn block)

instance VoidAnn SwitchBlock where
  removeAnn (SwitchBlock _ sLabel bStms) =
    SwitchBlock () (removeAnn sLabel) (fRem bStms)

instance VoidAnn SwitchLabel where
  removeAnn (SwitchCase _ exp) = SwitchCase () (removeAnn exp)
  removeAnn (Default _) = Default ()

instance VoidAnn ForInit where
  removeAnn (ForLocalVars _ mods typ vDecls) =
    ForLocalVars () (fRem mods) (removeAnn typ) (fRem vDecls)
  removeAnn (ForInitExps _ exps) = ForInitExps () (fRem exps)


instance VoidAnn ExceptionSpec where
  removeAnn (ExceptionSpec _ mods excTyp) =
    ExceptionSpec () (fRem mods) (removeAnn excTyp)

instance VoidAnn Exp where
  removeAnn (Lit _ lit)        = Lit () (removeAnn lit)
  removeAnn (ClassLit _ typ)   = ClassLit () (fRem typ)
  removeAnn (This _)           = This ()
  removeAnn (ThisClass _ name) = ThisClass () (removeAnn name)
  removeAnn (Paren _ exp)      = Paren () (removeAnn exp)
  removeAnn (InstanceCreation _ tArgs cType args cBody) =
    InstanceCreation () (fRem tArgs) (removeAnn cType) (fRem args)
      (fRem cBody)

  removeAnn (QualInstanceCreation _ exp tArgs ident args cBody) =
    QualInstanceCreation () (removeAnn exp) (fRem tArgs) (removeAnn ident)
      (fRem args) (fRem cBody)

  removeAnn (ArrayCreate _ typ expnPols mPols) =
    ArrayCreate () (removeAnn typ)
      (map (\(a,b) -> (removeAnn a,fRem b)) expnPols)
      (map (fRem) mPols)

  removeAnn (ArrayCreateInit _ t pols aInit) =
    ArrayCreateInit () (removeAnn t) (map (fRem) pols)
      (removeAnn aInit)

  removeAnn (FieldAccess _ fAccess) = FieldAccess () (removeAnn fAccess)
  removeAnn (MethodInv _ mInvoke)   = MethodInv () (removeAnn mInvoke)
  removeAnn (ArrayAccess _ aIndex)  = ArrayAccess () (removeAnn aIndex)
  removeAnn (ExpName _ name)        = ExpName () (removeAnn name)
  removeAnn (PostIncrement _ exp)   = PostIncrement () (removeAnn exp)
  removeAnn (PostDecrement _ exp)   = PostDecrement () (removeAnn exp)
  removeAnn (PreIncrement  _ exp)   = PreIncrement () (removeAnn exp)
  removeAnn (PreDecrement  _ exp)   = PreDecrement () (removeAnn exp)
  removeAnn (PrePlus  _ exp)        = PrePlus () (removeAnn exp)
  removeAnn (PreMinus _ exp)        = PreMinus () (removeAnn exp)
  removeAnn (PreBitCompl _ exp)     = PreBitCompl () (removeAnn exp)
  removeAnn (PreNot  _ exp)         = PreNot () (removeAnn exp)
  removeAnn (Cast _ typ exp)        = Cast () (removeAnn typ) (removeAnn exp)
  removeAnn (BinOp _ e1 op e2)      =
    BinOp () (removeAnn e1) (removeAnn op) (removeAnn e2)
  removeAnn (InstanceOf _ exp refT) =
    InstanceOf () (removeAnn exp) (removeAnn refT)
  removeAnn (Cond _ e1 e2 e3)       =
    Cond () (removeAnn e1) (removeAnn e2) (removeAnn e3)
  removeAnn (Assign _ lhs aOp exp)  =
    Assign () (removeAnn lhs) (removeAnn aOp) (removeAnn exp)

-- -- Paragon specific
--     | PolicyExp (XExp x) (PolicyExp x)
--     | LockExp (XExp x) (Lock x)

-- -- Quasi-quotation
--     | AntiQExp (XExp x) String

instance VoidAnn Literal  where
  removeAnn (Int     _ i) = Int     () i
  removeAnn (Word    _ i) = Word    () i
  removeAnn (Float   _ d) = Float   () d
  removeAnn (Double  _ d) = Double  () d
  removeAnn (Boolean _ b) = Boolean () b
  removeAnn (Char    _ c) = Char    () c
  removeAnn (String  _ s) = String  () s
  removeAnn (Null    _  ) = Null    ()

instance VoidAnn Op where
  removeAnn (Mult _)    = Mult () 
  removeAnn (Div _)     = Div () 
  removeAnn (Rem _)     = Rem ()
  removeAnn (Add _)     = Add ()
  removeAnn (Sub _)     = Sub ()
  removeAnn (LShift _)  = LShift ()
  removeAnn (RShift _)  = RShift ()
  removeAnn (RRShift _) = RRShift ()
  removeAnn (LThan _)   = LThan ()
  removeAnn (GThan _)   = GThan ()
  removeAnn (LThanE _)  = LThanE ()
  removeAnn (GThanE _)  = GThanE ()
  removeAnn (Equal _)   = Equal ()
  removeAnn (NotEq _)   = NotEq ()
  removeAnn (And _)     = And ()
  removeAnn (Or _)      = Or ()
  removeAnn (Xor _)     = Xor ()
  removeAnn (CAnd _)    = CAnd ()
  removeAnn (COr _)     = COr ()

instance VoidAnn AssignOp where
  removeAnn (EqualA _)   = EqualA ()
  removeAnn (MultA _)    = MultA () 
  removeAnn (DivA _)     = DivA () 
  removeAnn (RemA _)     = RemA ()
  removeAnn (AddA _)     = AddA ()
  removeAnn (SubA _)     = SubA ()
  removeAnn (LShiftA _)  = LShiftA ()
  removeAnn (RShiftA _)  = RShiftA ()
  removeAnn (RRShiftA _) = RRShiftA ()
  removeAnn (AndA _)     = AndA ()
  removeAnn (XorA _)     = XorA ()
  removeAnn (OrA _)      = OrA ()

instance VoidAnn Lhs where
  removeAnn (NameLhs _ name)     = NameLhs () (removeAnn name)
  removeAnn (FieldLhs _ fAccess) = FieldLhs () (removeAnn fAccess)
  removeAnn (ArrayLhs _ aIndex)  = ArrayLhs () (removeAnn aIndex)


instance VoidAnn ArrayIndex where
  removeAnn (ArrayIndex _ e1 e2) = ArrayIndex () (removeAnn e1) (removeAnn e2)

instance VoidAnn FieldAccess where
  removeAnn (PrimaryFieldAccess _ exp ident) =
    PrimaryFieldAccess () (removeAnn exp) (removeAnn ident)
  removeAnn (SuperFieldAccess   _ ident) = SuperFieldAccess () (removeAnn ident)
  removeAnn (ClassFieldAccess   _ name ident) =
    ClassFieldAccess   () (removeAnn name) (removeAnn ident)

instance VoidAnn MethodInvocation where
  removeAnn (MethodCallOrLockQuery _ name args) =
    MethodCallOrLockQuery () (removeAnn name) (fRem args)
  removeAnn (PrimaryMethodCall _ exp nwtargs ident args) =
    PrimaryMethodCall () (removeAnn exp) (fRem nwtargs) (removeAnn ident)
      (fRem args)
  removeAnn (SuperMethodCall _ nwtargs ident args) =
    SuperMethodCall () (fRem nwtargs) (removeAnn ident) (fRem args)
  removeAnn (ClassMethodCall _ name nwtargs ident args) =
    ClassMethodCall () (removeAnn name) (fRem nwtargs) (removeAnn ident)
      (fRem args)
  removeAnn (TypeMethodCall _ name nwtargs ident args) =
    TypeMethodCall () (removeAnn name) (fRem nwtargs) (removeAnn ident)
      (fRem args)


instance VoidAnn ArrayInit where
  removeAnn (ArrayInit _ vInits) = ArrayInit () (fRem vInits)


instance VoidAnn ReturnType where
  removeAnn (VoidType _) = VoidType ()
  removeAnn (LockType _) = LockType ()
  removeAnn (Type _ typ) = Type () (removeAnn typ)

instance VoidAnn Type where
  removeAnn (PrimType  _ pType) = PrimType  () (removeAnn pType)
  removeAnn (RefType   _ rType) = RefType   () (removeAnn rType)
  removeAnn (AntiQType _ s)     = AntiQType () s

instance VoidAnn RefType where
  removeAnn (ClassRefType _ cType) = ClassRefType () (removeAnn cType)
  removeAnn (TypeVariable _ ident) = TypeVariable () (removeAnn ident)
  removeAnn (ArrayType _ t mPols)  =
    ArrayType () (removeAnn t) (map (fRem) mPols)

instance VoidAnn ClassType where
  removeAnn (ClassType _ name tArgs) =
    ClassType () (removeAnn name) (fRem tArgs)

instance VoidAnn TypeArgument where
  removeAnn (Wildcard  _ wildCB)    = Wildcard  () (fRem wildCB)
  removeAnn (ActualArg _ nWildTArg) = ActualArg () (removeAnn nWildTArg)

instance VoidAnn NonWildTypeArgument where
  removeAnn (ActualName _ name)       = ActualName () (removeAnn name)
  removeAnn (ActualType _ rType)      = ActualType () (removeAnn rType)
  removeAnn (ActualExp _ eType)       = ActualExp () (removeAnn eType)
  removeAnn (ActualLockState _ locks) = ActualLockState () (fRem locks)

instance VoidAnn WildcardBound where
  removeAnn (ExtendsBound _ rType) = ExtendsBound () (removeAnn rType)
  removeAnn (SuperBound _ rType)   = SuperBound () (removeAnn rType)

instance VoidAnn PrimType where
  removeAnn (BooleanT _) = BooleanT ()
  removeAnn (ByteT    _) = ByteT    ()
  removeAnn (ShortT   _) = ShortT   ()
  removeAnn (IntT     _) = IntT     ()
  removeAnn (LongT    _) = LongT    ()
  removeAnn (CharT    _) = CharT    ()
  removeAnn (FloatT   _) = FloatT   ()
  removeAnn (DoubleT  _) = DoubleT  ()
--Paragon
  removeAnn (ActorT   _) = ActorT   ()
  removeAnn (PolicyT  _) = PolicyT  ()

instance VoidAnn TypeParam where
  removeAnn (TypeParam _ ident refTypes) =
    TypeParam () (removeAnn ident) (fRem refTypes)
-- -- Paragon specific
--                  | ActorParam    (XTypeParam x) (RefType x) (Ident x)
--                  | PolicyParam   (XTypeParam x) (Ident x)
--                  | LockStateParam(XTypeParam x) (Ident x)

instance VoidAnn ClauseVarDecl where
  removeAnn  (ClauseVarDecl _ rType ident) =
    ClauseVarDecl () (removeAnn rType) (removeAnn ident)

instance VoidAnn ClauseHead where
  removeAnn (ClauseDeclHead _ cVDecl) = ClauseDeclHead () (removeAnn cVDecl)
  removeAnn (ClauseVarHead _ actor)   = ClauseVarHead () (removeAnn actor)

instance VoidAnn Actor where
  removeAnn (Actor _ aNames) = Actor () (removeAnn aNames)
  removeAnn (Var   _ ident)  = Var () (removeAnn ident)

instance VoidAnn ActorName where
  removeAnn (ActorName _ name) = ActorName () (removeAnn name)
  removeAnn (ActorTypeVar _ rType ident) =
    ActorTypeVar () (removeAnn rType) (removeAnn ident)

instance VoidAnn Atom where
  removeAnn (Atom _ name acts) = Atom () (removeAnn name) (fRem acts)

instance VoidAnn Lock where
  removeAnn (Lock _ name aNames) = Lock () (removeAnn name) (fRem aNames)
  removeAnn (LockVar _ ident)    = LockVar () (removeAnn ident)

instance VoidAnn Ident where
  removeAnn (Ident _ bString) = Ident () bString
  removeAnn (AntiQIdent _ s)  = AntiQIdent () s

instance VoidAnn Name where
  removeAnn (Name _ nType mName ident) =
    Name () nType (fRem mName) (removeAnn ident)
  removeAnn (AntiQName _ s) = AntiQName () s
