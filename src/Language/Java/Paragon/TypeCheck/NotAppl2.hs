{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module Language.Java.Paragon.TypeCheck.NotAppl2 where
import Language.Java.Paragon.Syntax
import Language.Java.Paragon.TypeCheck.Types
import Language.Java.Paragon.Decorations.PteDecoration

import Prelude hiding (exp)

-- | NotAppl2 provides a function for removing annotations.
-- Example usage is, Constructor (TypeFam x) -> Constructor ()
class NotAppl2 a where
  notAppl2 :: a TC -> a PTE

-- Instances for the data types that currently needs to remove
-- annotations.

instance NotAppl2 PackageDecl where
  notAppl2 (PackageDecl _ name) = PackageDecl () (notAppl2 name)

instance NotAppl2 ImportDecl where
  notAppl2 (SingleTypeImport     _ name) =
    SingleTypeImport     () (notAppl2 name)
  notAppl2 (TypeImportOnDemand   _ name) =
    TypeImportOnDemand   () (notAppl2 name)
  notAppl2 (SingleStaticImport   _ name ident) =
    SingleStaticImport   () (notAppl2 name) (notAppl2 ident)
  notAppl2 (StaticImportOnDemand _ name) =
    StaticImportOnDemand () (notAppl2 name)

instance NotAppl2 ClassDecl where
  notAppl2 (ClassDecl _ mods ident tPars cType cTypes cBody) =
    ClassDecl () (notAppl2 <$> mods) (notAppl2 ident) (notAppl2 <$> tPars)
      (notAppl2 <$> cType) (notAppl2 <$> cTypes) (notAppl2 cBody)

--   notAppl2 (EnumDecl _ mods ident cTypes enumB) =
--     EnumDecl () (notAppl2 <$> mods) (notAppl2 ident) (notAppl2 <$> cTypes)
--       (notAppl2 enumB)

instance NotAppl2 ClassBody where
  notAppl2 (ClassBody _ decls) = ClassBody () (notAppl2 <$> decls)

-- instance NotAppl2 EnumBody where
--   notAppl2 (EnumBody _ eConsts decls) =
--     EnumBody () (notAppl2 <$> eConsts) (notAppl2 <$> decls)

-- instance NotAppl2 EnumConstant where
--   notAppl2 (EnumConstant _ ident args cBody) =
--     EnumConstant () (notAppl2 ident) (notAppl2 <$> args) (notAppl2 <$> cBody)

instance NotAppl2 InterfaceDecl where
  notAppl2 (InterfaceDecl _ mods ident tParams cTypes ifaceB) =
    InterfaceDecl () (notAppl2 <$> mods) (notAppl2 ident) (notAppl2 <$> tParams)
      (notAppl2 <$> cTypes) (notAppl2 ifaceB)

instance NotAppl2 InterfaceBody where
  notAppl2 (InterfaceBody _ mDecls) = InterfaceBody () (notAppl2 <$> mDecls)

instance NotAppl2 Decl where
  notAppl2 (MemberDecl _ mDecl) = MemberDecl () (notAppl2 mDecl)
  notAppl2 (InitDecl _ b block) = InitDecl () b (notAppl2 block)

instance NotAppl2 MemberDecl where
  notAppl2 (FieldDecl _ mods typ vDecl) =
    FieldDecl () (notAppl2 <$> mods) (notAppl2 typ) (notAppl2 <$> vDecl)
  notAppl2 (MethodDecl _ mods tPars rTyp ident fPars exSpecs mBody) =
    MethodDecl () (notAppl2 <$> mods) (notAppl2 <$> tPars) (notAppl2 rTyp)
      (notAppl2 ident) (notAppl2 <$> fPars) (notAppl2 <$> exSpecs) (notAppl2 mBody)
  notAppl2 (ConstructorDecl _ mods tPars ident fPars exSpecs cBody) =
    ConstructorDecl () (notAppl2 <$> mods) (notAppl2 <$> tPars) (notAppl2 ident)
      (notAppl2 <$> fPars) (notAppl2 <$> exSpecs) (notAppl2 cBody)
  notAppl2 (MemberClassDecl _ cDecl) = MemberClassDecl () (notAppl2 cDecl)
  notAppl2 (MemberInterfaceDecl _ iFaceDecl) =
    MemberInterfaceDecl () (notAppl2 iFaceDecl)

-- -- Paragon specific
--     | LockDecl (XMemberDecl x) [Modifier x]  (Ident x) [RefType x] (Maybe (LockProperties x))


instance NotAppl2 VarDecl where
  notAppl2 (VarDecl _ vDeclId vInit) =
    VarDecl () (notAppl2 vDeclId) (notAppl2 <$> vInit)

instance NotAppl2 VarDeclId where
  notAppl2 (VarId _ ident)          = VarId () (notAppl2 ident)
  notAppl2 (VarDeclArray _ vDeclId) = VarDeclArray () (notAppl2 vDeclId)

instance NotAppl2 VarInit where
  notAppl2 (InitExp _ exp)      = InitExp () (notAppl2 exp)
  notAppl2 (InitArray _ arInit) = error "notAppl2: case InitArray not implemented" --InitArray () (notAppl2 arInit)

instance NotAppl2 FormalParam where
  notAppl2 (FormalParam _ mods typ b vDeclId) =
    FormalParam () (notAppl2 <$> mods) (notAppl2 typ) b (notAppl2 vDeclId)

instance NotAppl2 MethodBody where
  notAppl2 (MethodBody _ block) = MethodBody () (notAppl2 <$> block)

instance NotAppl2 ConstructorBody where
  notAppl2 (ConstructorBody _ eCInv blockStms) =
    ConstructorBody () (notAppl2 <$> eCInv) (notAppl2 <$> blockStms)


instance NotAppl2 ExplConstrInv where
  notAppl2 (ThisInvoke _ nwta arg) = ThisInvoke () (notAppl2 <$> nwta) (notAppl2 <$> arg)
  notAppl2 (SuperInvoke _ nwta arg) =
    SuperInvoke () (notAppl2 <$> nwta) (notAppl2 <$> arg)
  notAppl2 (PrimarySuperInvoke _ exp nwta arg) =
    PrimarySuperInvoke () (notAppl2 exp) (notAppl2 <$> nwta) (notAppl2 <$> arg)

instance NotAppl2 Modifier where
  notAppl2 (Public    _) = Public ()
  notAppl2 (Private   _) = Private ()
  notAppl2 (Protected _) = Protected ()
  notAppl2 (Abstract  _) = Abstract ()
  notAppl2 (Final     _) = Final ()
  notAppl2 (Static    _) = Static ()
  notAppl2 (StrictFP  _) = StrictFP ()
  notAppl2 (Transient _) = Transient ()
  notAppl2 (Volatile  _) = Volatile ()
  notAppl2 (Native    _) = Native ()
-- Paragon specific
  notAppl2 (Typemethod _) = Typemethod ()
  notAppl2 (Reflexive  _) = Reflexive ()
  notAppl2 (Transitive _) = Transitive ()
  notAppl2 (Symmetric  _) = Symmetric ()
  notAppl2 (Readonly   _) = Readonly ()
  notAppl2 (Notnull    _) = Notnull ()

  notAppl2 (Reads   _ p ) = Reads   () (notAppl2 p) 
  notAppl2 (Writes  _ p ) = Writes  () (notAppl2 p) 
  notAppl2 (Opens   _ ls) = Opens   () (notAppl2 <$> ls)
  notAppl2 (Closes  _ ls) = Closes  () (notAppl2 <$> ls)
  notAppl2 (Expects _ ls) = Expects () (notAppl2 <$> ls) 

instance NotAppl2 Block where
  notAppl2 (Block _ bStms) = Block () (notAppl2 <$> bStms)

instance NotAppl2 BlockStmt where
  notAppl2 (BlockStmt _ stm)             = BlockStmt () (notAppl2 stm)
  notAppl2 (LocalClass _ cDecl)          = LocalClass () (notAppl2 cDecl)
  notAppl2 (LocalVars _ mods typ vDecls) =
    LocalVars () (notAppl2 <$> mods) (notAppl2 typ) (notAppl2 <$> vDecls)

-- -- -- Paragon specific
-- --     | LocalLock (XBlockStm x) [Modifier x] (Ident x) [RefType x]
-- --       (Maybe (LockProperties x))

instance NotAppl2 Stmt where
  notAppl2 (StmtBlock _ block) = StmtBlock () (notAppl2 block)
  notAppl2 (IfThen _ exp stm) =
    IfThen () (notAppl2 exp) (notAppl2 stm)
  notAppl2 (IfThenElse _ exp s1 s2) =
    IfThenElse () (notAppl2 exp) (notAppl2 s1) (notAppl2 s2)
  notAppl2 (While _ exp stm) = While () (notAppl2 exp) (notAppl2 stm)
  notAppl2 (BasicFor _ mfInit mExp mExps stm) =
    BasicFor () (notAppl2 <$> mfInit) (notAppl2 <$> mExp)
      (fmap notAppl2 <$> mExps) (notAppl2 stm)

  notAppl2 (EnhancedFor _ mods typ ident exp stm) =
    EnhancedFor () (notAppl2 <$> mods) (notAppl2 typ) (notAppl2 ident)
      (notAppl2 exp) (notAppl2 stm)

  notAppl2 (Empty _)                  = Empty ()
  notAppl2 (ExpStmt _ exp)            = ExpStmt () (notAppl2 exp)
  notAppl2 (Assert _ exp mExp)        =
    Assert () (notAppl2 exp) (notAppl2 <$> mExp)
  notAppl2 (Switch _ exp sBlocks)     =
    Switch () (notAppl2 exp) (notAppl2 <$> sBlocks)
  notAppl2 (Do _ stm exp)             = Do () (notAppl2 stm) (notAppl2 exp)
  notAppl2 (Break _ ident)            = Break () (notAppl2 <$> ident)
  notAppl2 (Continue _ ident)         = Continue () (notAppl2 <$> ident)
  notAppl2 (Return _ exp)             = Return () (notAppl2 <$> exp)
  notAppl2 (Synchronized _ exp block) =
    Synchronized () (notAppl2 exp) (notAppl2 block)
  notAppl2 (Throw _ exp)              = Throw () (notAppl2 exp)
  notAppl2 (Try _ block cs mBlock)    =
    Try () (notAppl2 block) (notAppl2 <$> cs) (notAppl2 <$> mBlock)
  notAppl2 (Labeled _ ident stmt)     =
    Labeled () (notAppl2 ident) (notAppl2 stmt)

-- -- -- Paragon specific
-- --     | Open  (XStm x) (Lock x)
-- --     | Close (XStm x) (Lock x)
-- --     | OpenBlock  (XStm x) (Lock x) (Block x)
-- --     | CloseBlock (XStm x) (Lock x) (Block x)



instance NotAppl2 Catch where
  notAppl2 (Catch _ fParam block) =
    Catch () (notAppl2 fParam) (notAppl2 block)

instance NotAppl2 SwitchBlock where
  notAppl2 (SwitchBlock _ sLabel bStms) =
    SwitchBlock () (notAppl2 sLabel) (notAppl2 <$> bStms)

instance NotAppl2 SwitchLabel where
  notAppl2 (SwitchCase _ exp) = SwitchCase () (notAppl2 exp)
  notAppl2 (Default _) = Default ()

instance NotAppl2 ForInit where
  notAppl2 (ForLocalVars _ mods typ vDecls) =
    ForLocalVars () (notAppl2 <$> mods) (notAppl2 typ) (notAppl2 <$> vDecls)
  notAppl2 (ForInitExps _ exps) = ForInitExps () (notAppl2 <$> exps)


instance NotAppl2 ExceptionSpec where
  notAppl2 (ExceptionSpec _ mods excTyp) =
    ExceptionSpec () (notAppl2 <$> mods) (notAppl2 excTyp)

instance NotAppl2 Exp where
  notAppl2 (Lit _ lit)        = Lit () (notAppl2 lit)
  notAppl2 (ClassLit _ typ)   = ClassLit () (notAppl2 <$> typ)
  notAppl2 (This _)           = This ()
  notAppl2 (ThisClass _ name) = ThisClass () (notAppl2 name)
  notAppl2 (Paren _ exp)      = Paren () (notAppl2 exp)
  notAppl2 (InstanceCreation _ tArgs cType args cBody) =
    InstanceCreation () (notAppl2 <$> tArgs) (notAppl2 cType) (notAppl2 <$> args)
      (notAppl2 <$> cBody)

  notAppl2 (QualInstanceCreation _ exp tArgs ident args cBody) =
    QualInstanceCreation () (notAppl2 exp) (notAppl2 <$> tArgs) (notAppl2 ident)
      (notAppl2 <$> args) (notAppl2 <$> cBody)

  notAppl2 (ArrayCreate _ typ expnPols mPols) =
    ArrayCreate () (notAppl2 typ)
      (map (\(a,b) -> (notAppl2 a,notAppl2 <$> b)) expnPols)
      (map (notAppl2 <$>) mPols)

  notAppl2 (ArrayCreateInit _ t pols aInit) =
    ArrayCreateInit () (notAppl2 t) (map (notAppl2 <$>) pols)
      (notAppl2 aInit)

  notAppl2 (FieldAccess _ fAccess) = FieldAccess () (notAppl2 fAccess)
  notAppl2 (MethodInv _ mInvoke)   = MethodInv () (notAppl2 mInvoke)
  notAppl2 (ArrayAccess _ aIndex)  = ArrayAccess () (notAppl2 aIndex)
  notAppl2 (ExpName _ name)        = ExpName () (notAppl2 name)
  notAppl2 (PostIncrement _ exp)   = PostIncrement () (notAppl2 exp)
  notAppl2 (PostDecrement _ exp)   = PostDecrement () (notAppl2 exp)
  notAppl2 (PreIncrement  _ exp)   = PreIncrement () (notAppl2 exp)
  notAppl2 (PreDecrement  _ exp)   = PreDecrement () (notAppl2 exp)
  notAppl2 (PrePlus  _ exp)        = PrePlus () (notAppl2 exp)
  notAppl2 (PreMinus _ exp)        = PreMinus () (notAppl2 exp)
  notAppl2 (PreBitCompl _ exp)     = PreBitCompl () (notAppl2 exp)
  notAppl2 (PreNot  _ exp)         = PreNot () (notAppl2 exp)
  notAppl2 (Cast _ typ exp)        = Cast () (notAppl2 typ) (notAppl2 exp)
  notAppl2 (BinOp _ e1 op e2)      =
    BinOp () (notAppl2 e1) (notAppl2 op) (notAppl2 e2)
  notAppl2 (InstanceOf _ exp refT) =
    InstanceOf () (notAppl2 exp) (notAppl2 refT)
  notAppl2 (Cond _ e1 e2 e3)       =
    Cond () (notAppl2 e1) (notAppl2 e2) (notAppl2 e3)
  notAppl2 (Assign _ lhs aOp exp)  =
    Assign () (notAppl2 lhs) (notAppl2 aOp) (notAppl2 exp)

-- -- Paragon specific
  notAppl2 (PolicyExp _ pe) = PolicyExp () (notAppl2 pe)
  notAppl2 (LockExp _ l) = LockExp () (notAppl2 l)

-- -- Quasi-quotation
--     | AntiQExp (XExp x) String

instance NotAppl2 PolicyExp where
  notAppl2 (PolicyLit     _ cs) = PolicyLit () (notAppl2 <$> cs)
  notAppl2 (PolicyOf      _ i) = PolicyOf () (notAppl2 i)
  notAppl2 (PolicyThis    _) = PolicyThis ()
  notAppl2 (PolicyTypeVar _ i) = PolicyTypeVar () (notAppl2 i)

instance NotAppl2 Clause where
  notAppl2 (Clause _ cvds ch a) = Clause () (notAppl2 <$> cvds) (notAppl2 ch) (notAppl2 <$> a)

instance NotAppl2 Literal  where
  notAppl2 (Int     _ i) = Int     () i
  notAppl2 (Word    _ i) = Word    () i
  notAppl2 (Float   _ d) = Float   () d
  notAppl2 (Double  _ d) = Double  () d
  notAppl2 (Boolean _ b) = Boolean () b
  notAppl2 (Char    _ c) = Char    () c
  notAppl2 (String  _ s) = String  () s
  notAppl2 (Null    _  ) = Null    ()

instance NotAppl2 Op where
  notAppl2 (Mult _)    = Mult () 
  notAppl2 (Div _)     = Div () 
  notAppl2 (Rem _)     = Rem ()
  notAppl2 (Add _)     = Add ()
  notAppl2 (Sub _)     = Sub ()
  notAppl2 (LShift _)  = LShift ()
  notAppl2 (RShift _)  = RShift ()
  notAppl2 (RRShift _) = RRShift ()
  notAppl2 (LThan _)   = LThan ()
  notAppl2 (GThan _)   = GThan ()
  notAppl2 (LThanE _)  = LThanE ()
  notAppl2 (GThanE _)  = GThanE ()
  notAppl2 (Equal _)   = Equal ()
  notAppl2 (NotEq _)   = NotEq ()
  notAppl2 (And _)     = And ()
  notAppl2 (Or _)      = Or ()
  notAppl2 (Xor _)     = Xor ()
  notAppl2 (CAnd _)    = CAnd ()
  notAppl2 (COr _)     = COr ()

instance NotAppl2 AssignOp where
  notAppl2 (EqualA _)   = EqualA ()
  notAppl2 (MultA _)    = MultA () 
  notAppl2 (DivA _)     = DivA () 
  notAppl2 (RemA _)     = RemA ()
  notAppl2 (AddA _)     = AddA ()
  notAppl2 (SubA _)     = SubA ()
  notAppl2 (LShiftA _)  = LShiftA ()
  notAppl2 (RShiftA _)  = RShiftA ()
  notAppl2 (RRShiftA _) = RRShiftA ()
  notAppl2 (AndA _)     = AndA ()
  notAppl2 (XorA _)     = XorA ()
  notAppl2 (OrA _)      = OrA ()

instance NotAppl2 Lhs where
  notAppl2 (NameLhs _ name)     = NameLhs () (notAppl2 name)
  notAppl2 (FieldLhs _ fAccess) = FieldLhs () (notAppl2 fAccess)
  notAppl2 (ArrayLhs _ aIndex)  = ArrayLhs () (notAppl2 aIndex)


instance NotAppl2 ArrayIndex where
 notAppl2 (ArrayIndex _ e1 e2) = ArrayIndex () (notAppl2 e1) (notAppl2 e2)

instance NotAppl2 FieldAccess where
  notAppl2 (PrimaryFieldAccess _ exp ident) =
    PrimaryFieldAccess () (notAppl2 exp) (notAppl2 ident)
  notAppl2 (SuperFieldAccess   _ ident) = SuperFieldAccess () (notAppl2 ident)
  notAppl2 (ClassFieldAccess   _ name ident) =
    ClassFieldAccess   () (notAppl2 name) (notAppl2 ident)

instance NotAppl2 MethodInvocation where
  notAppl2 (MethodCallOrLockQuery _ name args) =
    MethodCallOrLockQuery () (notAppl2 name) (notAppl2 <$> args)
  notAppl2 (PrimaryMethodCall _ exp nwtargs ident args) =
    PrimaryMethodCall () (notAppl2 exp) (notAppl2 <$> nwtargs) (notAppl2 ident)
      (notAppl2 <$> args)
  notAppl2 (SuperMethodCall _ nwtargs ident args) =
    SuperMethodCall () (notAppl2 <$> nwtargs) (notAppl2 ident) (notAppl2 <$> args)
  notAppl2 (ClassMethodCall _ name nwtargs ident args) =
    ClassMethodCall () (notAppl2 name) (notAppl2 <$> nwtargs) (notAppl2 ident)
      (notAppl2 <$> args)
  notAppl2 (TypeMethodCall _ name nwtargs ident args) =
    TypeMethodCall () (notAppl2 name) (notAppl2 <$> nwtargs) (notAppl2 ident)
      (notAppl2 <$> args)


instance NotAppl2 ArrayInit where
  notAppl2 (ArrayInit _ vInits) = ArrayInit () (notAppl2 <$> vInits)


instance NotAppl2 ReturnType where
  notAppl2 (VoidType _) = VoidType ()
  notAppl2 (LockType _) = LockType ()
  notAppl2 (Type _ typ) = Type () (notAppl2 typ)

instance NotAppl2 Type where
  notAppl2 (PrimType  _ pType) = PrimType  () (notAppl2 pType)
  notAppl2 (RefType   _ rType) = RefType   () (notAppl2 rType)
  notAppl2 (AntiQType _ s)     = error "AntiQType should never occur in TypeCheck phase."

instance NotAppl2 RefType where
  notAppl2 (ClassRefType _ cType) = ClassRefType () (notAppl2 cType)
  notAppl2 (TypeVariable _ ident) = TypeVariable () (notAppl2 ident)
  notAppl2 (TcArrayType{})  = error "notAppl2: function not implmented for ArrayType"

instance NotAppl2 ClassType where
  notAppl2 (ClassType _ name tArgs) =
    ClassType () (notAppl2 name) (notAppl2 <$> tArgs)

instance NotAppl2 TypeArgument where
  notAppl2 (TypeArgumentExp{})    = error "notAppl2: not implemented for TypeArgument"

instance NotAppl2 NonWildTypeArgument where
  notAppl2 (ActualName _ name)       = ActualName () (notAppl2 name)
  notAppl2 (ActualType _ rType)      = ActualType () (notAppl2 rType)
  notAppl2 (ActualExp _ eType)       = ActualExp () (notAppl2 eType)
  notAppl2 (ActualLockState _ locks) = ActualLockState () (notAppl2 <$> locks)

-- instance NotAppl2 WildcardBound where
--   notAppl2 (ExtendsBound _ rType) = ExtendsBound () (notAppl2 rType)
--   notAppl2 (SuperBound _ rType)   = SuperBound () (notAppl2 rType)

instance NotAppl2 PrimType where
  notAppl2 (BooleanT _) = BooleanT ()
  notAppl2 (ByteT    _) = ByteT    ()
  notAppl2 (ShortT   _) = ShortT   ()
  notAppl2 (IntT     _) = IntT     ()
  notAppl2 (LongT    _) = LongT    ()
  notAppl2 (CharT    _) = CharT    ()
  notAppl2 (FloatT   _) = FloatT   ()
  notAppl2 (DoubleT  _) = DoubleT  ()
--Paragon
  notAppl2 (ActorT   _) = ActorT   ()
  notAppl2 (PolicyT  _) = PolicyT  ()

instance NotAppl2 TypeParam where
  notAppl2 (TypeParam _ ident refTypes) =
    TypeParam () (notAppl2 ident) (notAppl2 <$> refTypes)
-- -- Paragon specific
--                  | ActorParam    (XTypeParam x) (RefType x) (Ident x)
--                  | PolicyParam   (XTypeParam x) (Ident x)
--                  | LockStateParam(XTypeParam x) (Ident x)

instance NotAppl2 ClauseVarDecl where
  notAppl2  (ClauseVarDecl _ rType ident) =
    ClauseVarDecl () (notAppl2 rType) (notAppl2 ident)

instance NotAppl2 ClauseHead where
  notAppl2 (ClauseDeclHead _ cVDecl) = ClauseDeclHead () (notAppl2 cVDecl)
  notAppl2 (ClauseVarHead _ actor)   = ClauseVarHead () (notAppl2 actor)

instance NotAppl2 Actor where
  notAppl2 (Actor _ aNames) = Actor () (notAppl2 aNames)
  notAppl2 (Var   _ ident)  = Var () (notAppl2 ident)

instance NotAppl2 ActorName where
  notAppl2 (ActorName _ name) = ActorName () (notAppl2 name)
  notAppl2 (ActorTypeVar _ rType ident) =
    ActorTypeVar () (notAppl2 rType) (notAppl2 ident)

instance NotAppl2 Atom where
  notAppl2 (Atom _ name acts) = Atom () (notAppl2 name) (notAppl2 <$> acts)

instance NotAppl2 Lock where
  notAppl2 (Lock _ name aNames) = Lock () (notAppl2 name) (notAppl2 <$> aNames)
  notAppl2 (LockVar _ ident)    = LockVar () (notAppl2 ident)

instance NotAppl2 Ident where
  notAppl2 (Ident pos bString) = Ident pos bString
  notAppl2 (AntiQIdent pos s)  = AntiQIdent pos s

instance NotAppl2 Name where
  notAppl2 (Name sp nType mName ident) =
    Name sp nType (notAppl2 <$> mName) (notAppl2 ident)
  notAppl2 (AntiQName sp s) = AntiQName sp s
