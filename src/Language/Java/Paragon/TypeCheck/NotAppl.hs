{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module Language.Java.Paragon.TypeCheck.NotAppl where
import Language.Java.Paragon.Syntax
import Language.Java.Paragon.TypeCheck.Types
import Language.Java.Paragon.Decorations.PaDecoration
import Language.Java.Paragon.SourcePos (defaultPos)

import Prelude hiding (exp)

-- | NotAppl provides a function for removing annotations.
-- Example usage is, Constructor (TypeFam x) -> Constructor ()
class NotAppl a where
  notAppl :: a PA -> a TC

-- | Alias for removing annotations inside lists
fRem :: Functor f => NotAppl a => f (a x) -> f (a TC)
fRem = fRem

-- Instances for the data types that currently needs to remove
-- annotations.

instance NotAppl PackageDecl where
  notAppl (PackageDecl _ name) = PackageDecl () (notAppl name)

instance NotAppl ImportDecl where
  notAppl (SingleTypeImport     _ name) =
    SingleTypeImport     () (notAppl name)
  notAppl (TypeImportOnDemand   _ name) =
    TypeImportOnDemand   () (notAppl name)
  notAppl (SingleStaticImport   _ name ident) =
    SingleStaticImport   () (notAppl name) (notAppl ident)
  notAppl (StaticImportOnDemand _ name) =
    StaticImportOnDemand () (notAppl name)

-- instance NotAppl ClassDecl where
--   notAppl (ClassDecl _ mods ident tPars cType cTypes cBody) =
--     ClassDecl () (fRem mods) (notAppl ident) (fRem tPars)
--       (fRem cType) (fRem cTypes) (notAppl cBody)

--   notAppl (EnumDecl _ mods ident cTypes enumB) =
--     EnumDecl () (fRem mods) (notAppl ident) (fRem cTypes)
--       (notAppl enumB)

-- instance NotAppl ClassBody where
--   notAppl (ClassBody _ decls) = ClassBody () (fRem decls)

-- instance NotAppl EnumBody where
--   notAppl (EnumBody _ eConsts decls) =
--     EnumBody () (fRem eConsts) (fRem decls)

-- instance NotAppl EnumConstant where
--   notAppl (EnumConstant _ ident args cBody) =
--     EnumConstant () (notAppl ident) (fRem args) (fRem cBody)

-- instance NotAppl InterfaceDecl where
--   notAppl (InterfaceDecl _ mods ident tParams cTypes ifaceB) =
--     InterfaceDecl () (fRem mods) (notAppl ident) (fRem tParams)
--       (fRem cTypes) (notAppl ifaceB)

-- instance NotAppl InterfaceBody where
--   notAppl (InterfaceBody _ mDecls) = InterfaceBody () (fRem mDecls)

-- instance NotAppl Decl where
--   notAppl (MemberDecl _ mDecl) = MemberDecl () (notAppl mDecl)
--   notAppl (InitDecl _ b block) = InitDecl () b (notAppl block)

-- instance NotAppl MemberDecl where
--   notAppl (FieldDecl _ mods typ vDecl) =991497515
--     FieldDecl () (fRem mods) (notAppl typ) (fRem vDecl)
--   notAppl (MethodDecl _ mods tPars rTyp ident fPars exSpecs mBody) =
--     MethodDecl () (fRem mods) (fRem tPars) (notAppl rTyp)
--       (notAppl ident) (fRem fPars) (fRem exSpecs) (notAppl mBody)
--   notAppl (ConstructorDecl _ mods tPars ident fPars exSpecs cBody) =
--     ConstructorDecl () (fRem mods) (fRem tPars) (notAppl ident)
--       (fRem fPars) (fRem exSpecs) (notAppl cBody)
--   notAppl (MemberClassDecl _ cDecl) = MemberClassDecl () (notAppl cDecl)
--   notAppl (MemberInterfaceDecl _ iFaceDecl) =
--     MemberInterfaceDecl () (notAppl iFaceDecl)

-- -- Paragon specific
--     | LockDecl (XMemberDecl x) [Modifier x]  (Ident x) [RefType x] (Maybe (LockProperties x))


-- instance NotAppl VarDecl where
--   notAppl (VarDecl _ vDeclId vInit) =
--     VarDecl () (notAppl vDeclId) (fRem vInit)

instance NotAppl VarDeclId where
  notAppl (VarId _ ident)          = VarId () (notAppl ident)
  notAppl (VarDeclArray _ vDeclId) = VarDeclArray () (notAppl vDeclId)

-- instance NotAppl VarInit where
--   notAppl (InitExp _ exp)      = InitExp () (notAppl exp)
--   notAppl (InitArray _ arInit) = InitArray () (notAppl arInit)

instance NotAppl FormalParam where
  notAppl (FormalParam _ mods typ b vDeclId) =
    FormalParam () (fRem mods) (notAppl typ) b (notAppl vDeclId)

-- instance NotAppl MethodBody where
--   notAppl (MethodBody _ block) = MethodBody () (fRem block)

-- instance NotAppl ConstructorBody where
--   notAppl (ConstructorBody _ eCInv blockStms) =
--     ConstructorBody () (fRem eCInv) (fRem blockStms)


-- instance NotAppl ExplConstrInv where
--   notAppl (ThisInvoke _ nwta arg) = ThisInvoke () (fRem nwta) (fRem arg)
--   notAppl (SuperInvoke _ nwta arg) =
--     SuperInvoke () (fRem nwta) (fRem arg)
--   notAppl (PrimarySuperInvoke _ exp nwta arg) =
--     PrimarySuperInvoke () (notAppl exp) (fRem nwta) (fRem arg)

instance NotAppl Modifier where
  notAppl (Public    _) = Public ()
  notAppl (Private   _) = Private ()
  notAppl (Protected _) = Protected ()
  notAppl (Abstract  _) = Abstract ()
  notAppl (Final     _) = Final ()
  notAppl (Static    _) = Static ()
  notAppl (StrictFP  _) = StrictFP ()
  notAppl (Transient _) = Transient ()
  notAppl (Volatile  _) = Volatile ()
  notAppl (Native    _) = Native ()

-- -- Paragon specific
-- --   notAppl (Typemethod _) = Typemethod ()
-- --   notAppl (Reflexive  _) = Reflexive ()
-- --   notAppl (Transitive _) = Transitive ()
-- --   notAppl (Symmetric  _) = Symmetric ()
-- --   notAppl (Readonly   _) = Readonly ()
-- --   notAppl (Notnull    _) = Notnull ()

-- --  | Reads   (XMod x) (Policy x)
-- --  | Writes  (XMod x) (Policy x)
-- --  | Opens   (XMod x) [Lock x]
-- --  | Closes  (XMod x) [Lock x]
-- --  | Expects (XMod x) [Lock x]

-- instance NotAppl Block where
--   notAppl (Block _ bStms) = Block () (fRem bStms)

-- instance NotAppl BlockStmt where
--   notAppl (BlockStmt _ stm)             = BlockStmt () (notAppl stm)
--   notAppl (LocalClass _ cDecl)          = LocalClass () (notAppl cDecl)
--   notAppl (LocalVars _ mods typ vDecls) =
--     LocalVars () (fRem mods) (notAppl typ) (fRem vDecls)

-- -- -- Paragon specific
-- --     | LocalLock (XBlockStm x) [Modifier x] (Ident x) [RefType x]
-- --       (Maybe (LockProperties x))

-- instance NotAppl Stmt where
--   notAppl (StmtBlock _ block) = StmtBlock () (notAppl block)
--   notAppl (IfThen _ exp stm) =
--     IfThen () (notAppl exp) (notAppl stm)
--   notAppl (IfThenElse _ exp s1 s2) =
--     IfThenElse () (notAppl exp) (notAppl s1) (notAppl s2)
--   notAppl (While _ exp stm) = While () (notAppl exp) (notAppl stm)
--   notAppl (BasicFor _ mfInit mExp mExps stm) =
--     BasicFor () (fRem mfInit) (fRem mExp)
--       (fmap fRem mExps) (notAppl stm)

--   notAppl (EnhancedFor _ mods typ ident exp stm) =
--     EnhancedFor () (fRem mods) (notAppl typ) (notAppl ident)
--       (notAppl exp) (notAppl stm)

--   notAppl (Empty _)                  = Empty ()
--   notAppl (ExpStmt _ exp)            = ExpStmt () (notAppl exp)
--   notAppl (Assert _ exp mExp)        =
--     Assert () (notAppl exp) (fRem mExp)
--   notAppl (Switch _ exp sBlocks)     =
--     Switch () (notAppl exp) (fRem sBlocks)
--   notAppl (Do _ stm exp)             = Do () (notAppl stm) (notAppl exp)
--   notAppl (Break _ ident)            = Break () (fRem ident)
--   notAppl (Continue _ ident)         = Continue () (fRem ident)
--   notAppl (Return _ exp)             = Return () (fRem exp)
--   notAppl (Synchronized _ exp block) =
--     Synchronized () (notAppl exp) (notAppl block)
--   notAppl (Throw _ exp)              = Throw () (notAppl exp)
--   notAppl (Try _ block cs mBlock)    =
--     Try () (notAppl block) (fRem cs) (fRem mBlock)
--   notAppl (Labeled _ ident stmt)     =
--     Labeled () (notAppl ident) (notAppl stmt)

-- -- -- Paragon specific
-- --     | Open  (XStm x) (Lock x)
-- --     | Close (XStm x) (Lock x)
-- --     | OpenBlock  (XStm x) (Lock x) (Block x)
-- --     | CloseBlock (XStm x) (Lock x) (Block x)



-- instance NotAppl Catch where
--   notAppl (Catch _ fParam block) =
--     Catch () (notAppl fParam) (notAppl block)

-- instance NotAppl SwitchBlock where
--   notAppl (SwitchBlock _ sLabel bStms) =
--     SwitchBlock () (notAppl sLabel) (fRem bStms)

-- instance NotAppl SwitchLabel where
--   notAppl (SwitchCase _ exp) = SwitchCase () (notAppl exp)
--   notAppl (Default _) = Default ()

-- instance NotAppl ForInit where
--   notAppl (ForLocalVars _ mods typ vDecls) =
--     ForLocalVars () (fRem mods) (notAppl typ) (fRem vDecls)
--   notAppl (ForInitExps _ exps) = ForInitExps () (fRem exps)


instance NotAppl ExceptionSpec where
  notAppl (ExceptionSpec _ mods excTyp) =
    ExceptionSpec () (fRem mods) (notAppl excTyp)

-- instance NotAppl Exp where
--   notAppl (Lit _ lit)        = Lit () (notAppl lit)
--   notAppl (ClassLit _ typ)   = ClassLit () (fRem typ)
--   notAppl (This _)           = This ()
--   notAppl (ThisClass _ name) = ThisClass () (notAppl name)
--   notAppl (Paren _ exp)      = Paren () (notAppl exp)
--   notAppl (InstanceCreation _ tArgs cType args cBody) =
--     InstanceCreation () (fRem tArgs) (notAppl cType) (fRem args)
--       (fRem cBody)

--   notAppl (QualInstanceCreation _ exp tArgs ident args cBody) =
--     QualInstanceCreation () (notAppl exp) (fRem tArgs) (notAppl ident)
--       (fRem args) (fRem cBody)

--   notAppl (ArrayCreate _ typ expnPols mPols) =
--     ArrayCreate () (notAppl typ)
--       (map (\(a,b) -> (notAppl a,fRem b)) expnPols)
--       (map (fRem) mPols)

--   notAppl (ArrayCreateInit _ t pols aInit) =
--     ArrayCreateInit () (notAppl t) (map (fRem) pols)
--       (notAppl aInit)

--   notAppl (FieldAccess _ fAccess) = FieldAccess () (notAppl fAccess)
--   notAppl (MethodInv _ mInvoke)   = MethodInv () (notAppl mInvoke)
--   notAppl (ArrayAccess _ aIndex)  = ArrayAccess () (notAppl aIndex)
--   notAppl (ExpName _ name)        = ExpName () (notAppl name)
--   notAppl (PostIncrement _ exp)   = PostIncrement () (notAppl exp)
--   notAppl (PostDecrement _ exp)   = PostDecrement () (notAppl exp)
--   notAppl (PreIncrement  _ exp)   = PreIncrement () (notAppl exp)
--   notAppl (PreDecrement  _ exp)   = PreDecrement () (notAppl exp)
--   notAppl (PrePlus  _ exp)        = PrePlus () (notAppl exp)
--   notAppl (PreMinus _ exp)        = PreMinus () (notAppl exp)
--   notAppl (PreBitCompl _ exp)     = PreBitCompl () (notAppl exp)
--   notAppl (PreNot  _ exp)         = PreNot () (notAppl exp)
--   notAppl (Cast _ typ exp)        = Cast () (notAppl typ) (notAppl exp)
--   notAppl (BinOp _ e1 op e2)      =
--     BinOp () (notAppl e1) (notAppl op) (notAppl e2)
--   notAppl (InstanceOf _ exp refT) =
--     InstanceOf () (notAppl exp) (notAppl refT)
--   notAppl (Cond _ e1 e2 e3)       =
--     Cond () (notAppl e1) (notAppl e2) (notAppl e3)
--   notAppl (Assign _ lhs aOp exp)  =
--     Assign () (notAppl lhs) (notAppl aOp) (notAppl exp)

-- -- -- Paragon specific
-- --     | PolicyExp (XExp x) (PolicyExp x)
-- --     | LockExp (XExp x) (Lock x)

-- -- -- Quasi-quotation
-- --     | AntiQExp (XExp x) String

instance NotAppl Literal  where
  notAppl (Int     _ i) = Int     () i
  notAppl (Word    _ i) = Word    () i
  notAppl (Float   _ d) = Float   () d
  notAppl (Double  _ d) = Double  () d
  notAppl (Boolean _ b) = Boolean () b
  notAppl (Char    _ c) = Char    () c
  notAppl (String  _ s) = String  () s
  notAppl (Null    _  ) = Null    ()

-- instance NotAppl Op where
--   notAppl (Mult _)    = Mult () 
--   notAppl (Div _)     = Div () 
--   notAppl (Rem _)     = Rem ()
--   notAppl (Add _)     = Add ()
--   notAppl (Sub _)     = Sub ()
--   notAppl (LShift _)  = LShift ()
--   notAppl (RShift _)  = RShift ()
--   notAppl (RRShift _) = RRShift ()
--   notAppl (LThan _)   = LThan ()
--   notAppl (GThan _)   = GThan ()
--   notAppl (LThanE _)  = LThanE ()
--   notAppl (GThanE _)  = GThanE ()
--   notAppl (Equal _)   = Equal ()
--   notAppl (NotEq _)   = NotEq ()
--   notAppl (And _)     = And ()
--   notAppl (Or _)      = Or ()
--   notAppl (Xor _)     = Xor ()
--   notAppl (CAnd _)    = CAnd ()
--   notAppl (COr _)     = COr ()

-- instance NotAppl AssignOp where
--   notAppl (EqualA _)   = EqualA ()
--   notAppl (MultA _)    = MultA () 
--   notAppl (DivA _)     = DivA () 
--   notAppl (RemA _)     = RemA ()
--   notAppl (AddA _)     = AddA ()
--   notAppl (SubA _)     = SubA ()
--   notAppl (LShiftA _)  = LShiftA ()
--   notAppl (RShiftA _)  = RShiftA ()
--   notAppl (RRShiftA _) = RRShiftA ()
--   notAppl (AndA _)     = AndA ()
--   notAppl (XorA _)     = XorA ()
--   notAppl (OrA _)      = OrA ()

-- instance NotAppl Lhs where
--   notAppl (NameLhs _ name)     = NameLhs () (notAppl name)
--   notAppl (FieldLhs _ fAccess) = FieldLhs () (notAppl fAccess)
--   notAppl (ArrayLhs _ aIndex)  = ArrayLhs () (notAppl aIndex)


-- instance NotAppl ArrayIndex where
--   notAppl (ArrayIndex _ e1 e2) = ArrayIndex () (notAppl e1) (notAppl e2)

-- instance NotAppl FieldAccess where
--   notAppl (PrimaryFieldAccess _ exp ident) =
--     PrimaryFieldAccess () (notAppl exp) (notAppl ident)
--   notAppl (SuperFieldAccess   _ ident) = SuperFieldAccess () (notAppl ident)
--   notAppl (ClassFieldAccess   _ name ident) =
--     ClassFieldAccess   () (notAppl name) (notAppl ident)

-- instance NotAppl MethodInvocation where
--   notAppl (MethodCallOrLockQuery _ name args) =
--     MethodCallOrLockQuery () (notAppl name) (fRem args)
--   notAppl (PrimaryMethodCall _ exp nwtargs ident args) =
--     PrimaryMethodCall () (notAppl exp) (fRem nwtargs) (notAppl ident)
--       (fRem args)
--   notAppl (SuperMethodCall _ nwtargs ident args) =
--     SuperMethodCall () (fRem nwtargs) (notAppl ident) (fRem args)
--   notAppl (ClassMethodCall _ name nwtargs ident args) =
--     ClassMethodCall () (notAppl name) (fRem nwtargs) (notAppl ident)
--       (fRem args)
--   notAppl (TypeMethodCall _ name nwtargs ident args) =
--     TypeMethodCall () (notAppl name) (fRem nwtargs) (notAppl ident)
--       (fRem args)


-- instance NotAppl ArrayInit where
--   notAppl (ArrayInit _ vInits) = ArrayInit () (fRem vInits)


instance NotAppl ReturnType where
  notAppl (VoidType _) = VoidType ()
  notAppl (LockType _) = LockType ()
  notAppl (Type _ typ) = Type () (notAppl typ)

instance NotAppl Type where
  notAppl (PrimType  _ pType) = PrimType  () (primTypePaToTc pType)
  notAppl (RefType   _ rType) = RefType   () (notAppl rType)
  notAppl (AntiQType _ s)     = error "AntiQType should never occur in TypeCheck phase."

instance NotAppl RefType where
  notAppl (ClassRefType _ cType) = ClassRefType () (notAppl cType)
  notAppl (TypeVariable _ ident) = TypeVariable () (notAppl ident)
  notAppl (PaArrayType _ t mPols)  = error "notAppl: function not implmented for ArrayType"

instance NotAppl ClassType where
  notAppl (ClassType _ name tArgs) =
    ClassType () (notAppl name) (fRem tArgs)

instance NotAppl TypeArgument where
  notAppl (PaWildcard  _ wildCB)    = error "notAppl: not implemented for TypeArgument"
  notAppl (PaActualArg _ nWildTArg) = error "notAppl: not implemented for TypeArgument"

-- instance NotAppl NonWildTypeArgument where
--   notAppl (ActualName _ name)       = ActualName () (notAppl name)
--   notAppl (ActualType _ rType)      = ActualType () (notAppl rType)
--   notAppl (ActualExp _ eType)       = ActualExp () (notAppl eType)
--   notAppl (ActualLockState _ locks) = ActualLockState () (fRem locks)

-- instance NotAppl WildcardBound where
--   notAppl (ExtendsBound _ rType) = ExtendsBound () (notAppl rType)
--   notAppl (SuperBound _ rType)   = SuperBound () (notAppl rType)

-- instance NotAppl PrimType where
--   notAppl (BooleanT _) = BooleanT ()
--   notAppl (ByteT    _) = ByteT    ()
--   notAppl (ShortT   _) = ShortT   ()
--   notAppl (IntT     _) = IntT     ()
--   notAppl (LongT    _) = LongT    ()
--   notAppl (CharT    _) = CharT    ()
--   notAppl (FloatT   _) = FloatT   ()
--   notAppl (DoubleT  _) = DoubleT  ()
-- --Paragon
--   notAppl (ActorT   _) = ActorT   ()
--   notAppl (PolicyT  _) = PolicyT  ()

instance NotAppl TypeParam where
  notAppl (TypeParam _ ident refTypes) =
    TypeParam () (notAppl ident) (fRem refTypes)
-- -- Paragon specific
--                  | ActorParam    (XTypeParam x) (RefType x) (Ident x)
--                  | PolicyParam   (XTypeParam x) (Ident x)
--                  | LockStateParam(XTypeParam x) (Ident x)

-- instance NotAppl ClauseVarDecl where
--   notAppl  (ClauseVarDecl _ rType ident) =
--     ClauseVarDecl () (notAppl rType) (notAppl ident)

-- instance NotAppl ClauseHead where
--   notAppl (ClauseDeclHead _ cVDecl) = ClauseDeclHead () (notAppl cVDecl)
--   notAppl (ClauseVarHead _ actor)   = ClauseVarHead () (notAppl actor)

-- instance NotAppl Actor where
--   notAppl (Actor _ aNames) = Actor () (notAppl aNames)
--   notAppl (Var   _ ident)  = Var () (notAppl ident)

-- instance NotAppl ActorName where
--   notAppl (ActorName _ name) = ActorName () (notAppl name)
--   notAppl (ActorTypeVar _ rType ident) =
--     ActorTypeVar () (notAppl rType) (notAppl ident)

-- instance NotAppl Atom where
--   notAppl (Atom _ name acts) = Atom () (notAppl name) (fRem acts)

-- instance NotAppl Lock where
--   notAppl (Lock _ name aNames) = Lock () (notAppl name) (fRem aNames)
--   notAppl (LockVar _ ident)    = LockVar () (notAppl ident)

instance NotAppl Ident where
  notAppl (Ident pos bString) = Ident pos bString
  notAppl (AntiQIdent pos s)  = AntiQIdent pos s

instance NotAppl Name where
  notAppl (Name sp nType mName ident) =
    Name sp nType (fRem mName) (notAppl ident)
  notAppl (AntiQName sp s) = AntiQName sp s
