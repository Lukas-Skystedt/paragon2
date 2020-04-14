{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes, PatternGuards #-}
module Language.Java.Paragon.Compile (compileTransform) where
import Language.Java.Paragon.RemoveAnns
import Language.Java.Paragon.Syntax
import Language.Java.Paragon.Pretty
import Language.Java.Paragon.Error()
import Language.Java.Paragon.TypeCheck.Types
import Language.Java.Paragon.Interaction
import Language.Java.Paragon.Decorations.PaDecoration(PA)
import Language.Java.Paragon.Decorations.NoDecoration(UD)
import Language.Java.Paragon.SourcePos
import Data.Generics.Uniplate.Data

import Data.List (nub, union, delete)
--import Data.Maybe (fromJust)
import Control.Arrow ((***))

import qualified Data.ByteString.Char8 as B

compilerModule :: String
compilerModule = libraryBase ++ ".Compile"

compileTransform :: CompilationUnit PA -> CompilationUnit UD
compileTransform (CompilationUnit _ mp is tds) =
    CompilationUnit () (fmap removeAnn mp) (map removeAnn is) $ map compileTypeDecl tds

compileTypeDecl :: TypeDecl PA -> TypeDecl UD
compileTypeDecl td =
    case td of
      ClassTypeDecl     _ cdecl -> ClassTypeDecl     () $ compileClassDecl     cdecl
      InterfaceTypeDecl _ idecl -> InterfaceTypeDecl () $ compileInterfaceDecl idecl


compileInterfaceDecl :: InterfaceDecl PA -> InterfaceDecl UD
compileInterfaceDecl (InterfaceDecl _ ms i tps sups ibody) =
  let ms' = delete (Native ()) $ removeParagonMods ms -- No lockstate mods allowed here
                                                      -- In vanilla Java, native cannot appear on classes
      sups'  = map compileClassType_ sups
      (tps', _tpMembers, _tpPars, _tpAsss) = splitTypeParams tps
  in InterfaceDecl () ms' (removeAnn i) tps' sups' $
       compileInterfaceBody ibody

compileInterfaceBody :: InterfaceBody PA -> InterfaceBody UD
compileInterfaceBody (InterfaceBody _ mds) =
    InterfaceBody () $ map (compileSimpleMemberDecl [] []) mds

-- 1. Remove Paragon modifiers
-- 2. Transform Paragon type params into ordinary params
-- 3. Transform body
compileClassDecl :: ClassDecl PA -> ClassDecl UD
compileClassDecl (ClassDecl _ ms i tps mSuper impls cbody) =
  let ms' = delete (Native ()) $ removeParagonMods ms -- No lockstate mods allowed here
                                                      -- In vanilla Java, native cannot appear on classes
      mSuper' = fmap compileClassType_ mSuper
      impls'  = map compileClassType_ impls
      (tps', tpMembers, tpPars, tpAsss) = splitTypeParams tps
  in ClassDecl () ms' (removeAnn i) tps' mSuper' impls' $
       compileClassBody cbody tpMembers tpPars tpAsss
compileClassDecl _ = panic (compilerModule ++ ".compileClassDecl")
                     $ "Enum not supported"

-- Paragon type parameters need to be replaced by runtime counterparts.
-- 1. Lock state parameters should be removed completed.
-- 2. Actor and Policy parameters need to be around at runtime. Each
--    parameter is translated into:
--     a) a field of the parameterized class
--     b) a parameter to every constructor of the class
--     c) an assignment of the parameter b) to the field a)
--        at the beginning of every constructor of the class
splitTypeParams :: [TypeParam PA]
                -> ([TypeParam UD],[MemberDecl UD],[FormalParam UD],[BlockStmt UD])
splitTypeParams = go ([],[],[],[]) -- error "compileTypeParams undefined"
    where
      go (ttps,fds,fps,as) [] = (reverse ttps, reverse fds, reverse fps, reverse as)
      go (ttps,fds,fps,as) (tp:tps) =
          case tp of
            TypeParam{}      -> go (removeAnn tp:ttps,fds,fps,as) tps -- Retain
            LockStateParam{} -> go (           ttps,fds,fps,as) tps -- Ignore
            _ -> let (i,ty) =
                         case tp of
                           ActorParam  _ rt iP ->
                               (removeAnn iP, -- [typeQQ| se.chalmers.paragon.ConcreteActor |]
                                RefType () (removeAnn rt))    -- concreteActorType)

                           PolicyParam _ iP ->
                               (removeAnn iP, -- [typeQQ| se.chalmers.paragon.Policy        |]
                                policyType)
                           _ -> panic (compilerModule ++ ".splitTypeParams")
                                $ show tp
                     fd = --   [fieldDeclQQ| public final #T#ty #i; |]
                          FieldDecl () [Public (),Final ()] ty [VarDecl () (VarId () i) Nothing]
                     fp = -- [formalParamQQ| final #T#ty #i         |]
                          FormalParam () [Final ()] ty False (VarId () i)
                     a  = -- [blockStmtQQ| this.#i = #i;         |]
                          BlockStmt () (ExpStmt () (Assign ()
                             (FieldLhs () (PrimaryFieldAccess () (This ()) i))
                             (EqualA ()) (ExpName () (Name () EOrLName Nothing i))))
                 in go (ttps,fd:fds,fp:fps,a:as) tps


compileClassBody :: ClassBody PA -> [MemberDecl UD] -> [FormalParam UD] -> [BlockStmt UD] -> ClassBody UD
compileClassBody (ClassBody _ ds) tpMembers tpPars tpAsss =
  let ds' = concat $ map (compileDecl tpPars tpAsss) ds
  in ClassBody () (map (MemberDecl ()) tpMembers ++ ds')

compileDecl :: [FormalParam UD] -> [BlockStmt UD] -> Decl PA -> [Decl UD]
compileDecl _ _ (InitDecl _ _ _) = panic (compilerModule ++ ".compileDecl")
                                   $ "InitDecl not yet supported"
compileDecl tpPars tpAsss (MemberDecl _ md) = compileMemberDecl tpPars tpAsss md

compileMemberDecl :: [FormalParam UD] -> [BlockStmt UD] -> MemberDecl PA -> [Decl UD]
compileMemberDecl tpPars tpAsss md =
    case md of
      LockDecl {} -> compileLockDecl md
      _ -> (:[]) . MemberDecl () $ compileSimpleMemberDecl tpPars tpAsss md

compileSimpleMemberDecl :: [FormalParam UD] -> [BlockStmt UD] -> MemberDecl PA -> MemberDecl UD
compileSimpleMemberDecl tpPars tpAsss md =
    case md of
      -- Actors
      FieldDecl _ ms t vds -> compileVarDeclGeneric (FieldDecl ()) ms t vds

      MethodDecl _ ms tps rt i fps xs mb ->
          let ms' = removeParagonMods ms
              (tps', _, tpPs, _) = splitTypeParams tps
              rt' = compileReturnType rt
              fps' = map compileFormalParam fps
              xs' = map compileExn xs
          in MethodDecl () ms' tps' rt' (removeAnn i) (tpPs ++ fps') xs' $ compileMethodBody mb

      ConstructorDecl _ ms tps i fps xs cb ->
          let ms' = removeParagonMods ms
              (tps', _, tpPs, _) = splitTypeParams tps
              fps' = map compileFormalParam fps
              xs'  = map compileExn xs
              -- Add the downgraded type parameters: class parameters (tpPars) first,
              -- then parameters specific to this constructor (tpPs)
          in ConstructorDecl () ms' tps' (removeAnn i) (tpPars ++ tpPs ++ fps') xs'
                 $ compileConstrBody tpAsss cb

      _ -> panic (compilerModule ++ ".compileSimpleMemberDecl")
           $ prettyPrint md -- Locks should be filtered out already

compileConstrBody :: [BlockStmt UD] -> ConstructorBody PA -> ConstructorBody UD
compileConstrBody tpAsss (ConstructorBody _ meci bss) =
    -- Add the initialization of the downgraded type parameters
    ConstructorBody () (fmap compileECI meci) (tpAsss ++ map compileBlockStmt bss)

compileECI :: ExplConstrInv PA -> ExplConstrInv UD
compileECI (ThisInvoke  _ tas as) =
    let (trueTas, demotedArgs) = splitNWTypeArgs tas
    in ThisInvoke  () trueTas (demotedArgs ++ map compileExp as)
compileECI (SuperInvoke _ tas as) =
    let (trueTas, demotedArgs) = splitNWTypeArgs tas
    in SuperInvoke () trueTas (demotedArgs ++ map compileExp as)
compileECI (PrimarySuperInvoke _ e tas as) =
    let (trueTas, demotedArgs) = splitNWTypeArgs tas
    in PrimarySuperInvoke () (compileExp e) trueTas (demotedArgs ++ map compileExp as)

compileFormalParam :: FormalParam PA -> FormalParam UD
compileFormalParam (FormalParam _ ms t va vid) =
    FormalParam () (removeParagonMods ms) (compileType t) va (removeAnn vid)

actorVarDecl, policyVarDecl, compileVarDecl :: VarDecl PA -> VarDecl UD
actorVarDecl (VarDecl _ (VarId _ i@(Ident _ rawI)) Nothing)
    = -- [varDeclQQ| $$i = Actor.newConcreteActor($s$rawI) |]
      vDecl (removeAnn i) $ callStatic "Actor" "newConcreteActor"
                             [Lit () $ String () $ B.unpack rawI]
actorVarDecl vd = compileVarDecl vd

policyVarDecl (VarDecl _ (VarId _ i@(Ident _ rawI))
                           (Just (InitExp _ (PolicyExp _ (PolicyLit _ cs)))))
    = vDecl (removeAnn i) $ callStatic "Policy" "newPolicy"
      (Lit () (String () $ B.unpack rawI) : map clauseToExp cs)
policyVarDecl vd = compileVarDecl vd

compileVarDecl (VarDecl _ vid mInit) = VarDecl () (removeAnn vid) $ fmap compileVarInit mInit

compileExn :: ExceptionSpec PA -> ExceptionSpec UD
compileExn (ExceptionSpec _ _ms rt) = ExceptionSpec () [] -- no modifiers on exceptions in java!
                                       $ compileRefType rt

compileReturnType :: ReturnType PA -> ReturnType UD
compileReturnType (LockType _) = Type () $ PrimType () $ BooleanT ()
compileReturnType (Type _ t)   = Type () $ compileType t
compileReturnType rett = removeAnn rett

compileType :: Type PA -> Type UD
compileType (RefType _ rt) = RefType () $ compileRefType rt
compileType t@(PrimType _ pt) = case pt of
                                  ActorT _  -> concreteActorType
                                  PolicyT _ -> policyType
                                  _ -> removeAnn t
compileType t = removeAnn t

compileRefType :: RefType PA -> RefType UD
compileRefType (ArrayType _ t) =
    ArrayType () (compileType t) {-$ map (const Nothing) mps-} -- No policy parameters!
compileRefType (ClassRefType _ ct) = ClassRefType () $ compileClassType_ ct
compileRefType rt = removeAnn rt

compileClassType :: ClassType PA -> (ClassType UD, [Argument UD])
compileClassType (ClassType _ n tas) =
    let (trueTas, demotedArgs) = splitTypeArgs tas
    in (ClassType () (removeAnn n) trueTas, demotedArgs)

-- When we don't care about demoted policies and actors
compileClassType_ :: ClassType PA -> ClassType UD
compileClassType_ = fst . compileClassType

--compileTypeArgs :: [TypeArgument PA] -> [TypeArgument ()]
--compileTypeArgs _ = [] -- TODO: Cheating!!!

--compileNWTypeArgs :: [NonWildTypeArgument PA] -> [NonWildTypeArgument ()]
--compileNWTypeArgs _ = [] -- TODO: Cheating!!!

compileMethodBody :: MethodBody PA -> MethodBody UD
compileMethodBody (MethodBody _ (Just bl)) = MethodBody () . Just $ compileBlock bl
compileMethodBody mb = removeAnn mb

compileBlock :: Block PA -> Block UD
compileBlock (Block _ bss) = Block () $ map compileBlockStmt bss

compileBlockStmt :: BlockStmt PA -> BlockStmt UD
compileBlockStmt (BlockStmt _ stmt) = BlockStmt () $ compileStmt stmt
compileBlockStmt (LocalVars _ ms t vds) = compileVarDeclGeneric (LocalVars ()) ms t vds
compileBlockStmt bss = panic (compilerModule ++ ".compileBlockStmt")
                       $ prettyPrint bss

compileVarDeclGeneric :: ([Modifier UD] -> Type UD -> [VarDecl UD] -> res)
                       -> [Modifier PA] -> Type PA  -> [VarDecl PA] -> res
compileVarDeclGeneric con ms t vds =
    let (t', vds') = case t of
                       PrimType _ (PolicyT _) -> (policyType, map policyVarDecl vds)
--                       PrimType _ (ActorT  _) -> (concreteActorType, map actorVarDecl vds)
                       _ -> (compileType t, map compileVarDecl vds)
    in con (removeParagonMods ms)  t' vds'



compileStmt :: Stmt PA -> Stmt UD
compileStmt (StmtBlock _ bl) = StmtBlock () $ compileBlock bl
compileStmt (Open t (Lock _ lN aN )) = ExpStmt () $ MethodInv () $ MethodCallOrLockQuery ()
         (Name () MName (Just $ removeAnn lN)
                   (Ident () (B.pack "open")))
         $ map (compileExp . (\aname -> case aname of
                                          ActorName _ x -> ExpName t x
                                          ActorTypeVar _ _ i -> ExpName t (Name t EName Nothing i)
                             )) aN
compileStmt (Close t (Lock _ lN aN )) = ExpStmt () $ MethodInv () $ MethodCallOrLockQuery ()
         (Name () MName (Just $ removeAnn lN)
                   (Ident () (B.pack "close")))
         $ map (compileExp . (\(ActorName _ x) -> (ExpName t x))) aN
compileStmt (OpenBlock  _ _ bl) = StmtBlock () $ compileBlock bl
compileStmt (CloseBlock _ _ bl) = StmtBlock () $ compileBlock bl

compileStmt (IfThen _ e s) = IfThen () (compileExp e) (compileStmt s)
compileStmt (IfThenElse _ e th el) = IfThenElse () (compileExp e) (compileStmt th) (compileStmt el)
compileStmt (While _ e s) = While () (compileExp e) (compileStmt s)
compileStmt (BasicFor _ mIn mTest mUp s) =
    let mIn' = fmap compileForInit mIn
        mTest' = fmap compileExp mTest
        mUp' = fmap (map compileExp) mUp
    in BasicFor () mIn' mTest' mUp' $ compileStmt s
compileStmt (ExpStmt _ e) = ExpStmt () $ compileExp e
compileStmt (Return _ me) = Return () $ fmap compileExp me
compileStmt (Throw _ e) = Throw () $ compileExp e
compileStmt (Try _ bl cs mfin) = Try () (compileBlock bl) (map compileCatch cs) (fmap compileBlock mfin)
compileStmt st = removeAnn st

compileForInit :: ForInit PA -> ForInit UD
compileForInit (ForInitExps _ es) = ForInitExps () $ map compileExp es
compileForInit (ForLocalVars _ ms t vds) = compileVarDeclGeneric (ForLocalVars ()) ms t vds

compileCatch :: Catch PA -> Catch UD
compileCatch (Catch _ fp bl) = Catch () (compileFormalParam fp) (compileBlock bl)

compileVarInit :: VarInit PA -> VarInit UD
compileVarInit (InitExp   _ e ) = InitExp   () $ compileExp e
compileVarInit (InitArray _ ai) = InitArray () $ compileArrayInit ai

compileArrayInit :: ArrayInit PA -> ArrayInit UD
compileArrayInit (ArrayInit _ vis) = ArrayInit () $ map compileVarInit vis

--compileExp :: Exp PA -> Exp ()
--compileExp = transformBi compileExp'

-- TODO: Fill out full, since no longer generic
compileExp :: Exp PA -> Exp UD
compileExp (PolicyExp _ pe) = compilePolicyExp pe

--  For instance creation, we need to move type
-- arguments to actual arguments -- but not right now!
-- the part where we give isNative its value needs
-- to be adapted after the new type checking is
-- completed.
compileExp (InstanceCreation a tas ct args mcbody) =
    let isNative = False --maybe False snd a ---- ADAPT FOR NEW T TYPE
        (trueTas, demotedArgs) = splitTypeArgs tas
        (ct', classDemotedArgs) = compileClassType ct
        extraArgs = if isNative then [] else demotedArgs ++ classDemotedArgs
    -- Here we need to demote the args in the ct as well!!!
    in InstanceCreation () trueTas ct' (extraArgs ++ map compileExp args)
                         (fmap (\cb -> compileClassBody cb [] [] []) mcbody)
{-
compileExp (QualInstanceCreation e tas i args mcbody) = do
  undefined -}
compileExp (ArrayCreate _ t edims idims) =
    let edims' = map (compileExp *** const Nothing) edims
        idims' = map (const Nothing) idims
    in ArrayCreate () (compileType t) edims' idims'
compileExp e@(ArrayCreateInit{}) =
    error $ "Compilation of ArrayCreateInit not yet supported: " ++ prettyPrint e

compileExp (FieldAccess _ fa) = FieldAccess () $ compileFieldAccess fa
compileExp (MethodInv _ mi) = MethodInv () $ compileMethodInv mi
compileExp (ArrayAccess _ ai) = ArrayAccess () $ compileArrayIndex ai

compileExp (PostIncrement _ e) =  PostIncrement () $ compileExp e
compileExp (PreIncrement  _ e) =  PreIncrement  () $ compileExp e
compileExp (PostDecrement _ e) =  PostDecrement () $ compileExp e
compileExp (PreDecrement  _ e) =  PreDecrement  () $ compileExp e

compileExp (PrePlus     _ e) =  PrePlus     () $ compileExp e
compileExp (PreMinus    _ e) =  PreMinus    () $ compileExp e
compileExp (PreBitCompl _ e) =  PreBitCompl () $ compileExp e
compileExp (PreNot      _ e) =  PreNot      () $ compileExp e

compileExp (Cast _ t e) = Cast () (compileType t) (compileExp e)

-- Lock names must be handled in a special way - but not right now!
compileExp (ExpName _ n) = ExpName () $ removeAnn n
compileExp (LockExp _ l) = lockExpToExp l

-- Certain operators have special effects on paragon types
compileExp (BinOp _ e1 op e2) = compileBinOp (removeAnn op) e1 e2

compileExp (ClassLit _ mt) = ClassLit () (fmap compileType mt)
compileExp (Paren _ e) = Paren () $ compileExp e
compileExp (Cond _ c th el) = Cond () (compileExp c) (compileExp th) (compileExp el)

compileExp (Assign _ lhs aop e) = compileAOp (removeAnn aop) lhs e
--    Assign () (compileLhs lhs) (compileAOp aop) (compileExp e)

compileExp e@(InstanceOf{}) =
    error $ "Compilation of InstanceOf not yet supported: " ++ prettyPrint e

-- Lit, This, ThisClass, AntiQExp
compileExp e = removeAnn e


compileMethodInv :: MethodInvocation PA -> MethodInvocation UD
compileMethodInv mi = case mi of
  MethodCallOrLockQuery _ n@(Name _ LName _ _) as ->
      MethodCallOrLockQuery ()
         (Name () MName (Just $ removeAnn n)
                   (Ident () (B.pack "isOpen")))
         $ map compileExp as

  MethodCallOrLockQuery _ n as -> MethodCallOrLockQuery () (removeAnn n) $ map compileExp as
  PrimaryMethodCall _ e tas i as ->
      let isNative = maybe False snd $ ann mi
          (trueTas,demotedArgs) = splitNWTypeArgs tas
          args = (if isNative then id else (demotedArgs ++)) $ map compileExp as
       in PrimaryMethodCall () (compileExp e) trueTas (removeAnn i) args
  TypeMethodCall _ n tas i as ->
      let isNative = maybe False snd $ ann mi
          (trueTas,demotedArgs) = splitNWTypeArgs tas
          args = (if isNative then id else (demotedArgs ++)) $ map compileExp as
       in TypeMethodCall () (removeAnn n) trueTas (removeAnn i) args
  _ -> panic (compilerModule ++ ".compileMethodInv")
       $ prettyPrint mi

splitTypeArgs :: [TypeArgument PA] -> ([TypeArgument UD], [Argument UD])
splitTypeArgs = error "spilTypeArgs: type arguments not yet supported"
-- splitTypeArgs = go ([], [])
--     where
--       go (ttas, as) [] = (reverse ttas, reverse as)
--       go (ttas, as) (ta:tas) =
--           case ta of
--             Wildcard{} -> panic (compilerModule ++ ".splitTypeArgs") $
--                           "Wildcards not yet supported"
--                           -- go (compileWildcard ta:ttas, as) tas
--             ActualArg _ nwta ->
--                 case nwta of
--                   ActualType _ rt ->
--                       let ta' = ActualArg () $ ActualType () $ compileRefType rt
--                       in go (ta':ttas, as) tas
--                   ActualName _ (Name _ TName _ _) -> go (removeAnn ta:ttas, as) tas
--                   ActualName _ n@(Name _ EName _ _) -> go (ttas, ExpName () (removeAnn n) : as) tas
--                   ActualExp _ e -> go (ttas, compileExp e : as) tas
--                   -- Lock states and lock names have no runtime counterpart, so we just ignore those
--                   _ -> go (ttas, as) tas

splitNWTypeArgs :: [NonWildTypeArgument PA] -> ([NonWildTypeArgument UD], [Argument UD])
splitNWTypeArgs = go ([], [])
    where
      go (ttas, as) [] = (reverse ttas, reverse as)
      go (ttas, as) (nwta:tas) =
                case nwta of
                  ActualType{} -> go (removeAnn nwta:ttas, as) tas
                  ActualName _ (Name _ TName _ _) -> go (removeAnn nwta:ttas, as) tas
                  ActualName _ n@(Name _ EName _ _) -> go (ttas, ExpName () (removeAnn n) : as) tas
                  ActualExp _ e -> go (ttas, compileExp e : as) tas
                  -- Lock states and lock names have no runtime counterpart, so we just ignore those
                  _ -> go (ttas, as) tas

-- Compiling binary operators. The interesting cases are
-- the ones where the operands are policies.
compileBinOp :: Op UD -> Exp PA -> Exp PA -> Exp UD
compileBinOp op e1 e2
             | Just (t1, _) <- ann e1, t1 == policyT,
               Just (t2, _) <- ann e2, t2 == policyT,
               op `elem` [Mult (), Add ()] =
                   mkParagonPolicyOp op (compileExp e1) (compileExp e2)
compileBinOp op e1 e2 = BinOp () (compileExp e1) op (compileExp e2)

mkParagonPolicyOp :: Op UD -> Exp UD -> Exp UD -> Exp UD
mkParagonPolicyOp op e1 e2 =
    case op of
      Mult _ -> MethodInv () $
                 PrimaryMethodCall () e1 [] (Ident () (B.pack "join")) [e2]
      Add  _ -> MethodInv () $
                 PrimaryMethodCall () e1 [] (Ident () (B.pack "meet")) [e2]
      _ -> panic (compilerModule ++ ".mkParagonPolicyOp")
           $ "Unexpected operator: " ++ show op

mkParagonPolicyAssign :: AssignOp UD -> Lhs UD -> Exp UD -> Exp UD
mkParagonPolicyAssign aop lhs e =
    case aop of
      MultA _ -> MethodInv () $
                  PrimaryMethodCall () (lhsToExp lhs) []
                   (Ident () (B.pack "joinWith")) [e]
      AddA  _ -> MethodInv () $
                  PrimaryMethodCall () (lhsToExp lhs) []
                   (Ident () (B.pack "meetWith")) [e]
      _ -> Assign () lhs aop e

lhsToExp :: Lhs UD -> Exp UD
lhsToExp (NameLhs _ n) = ExpName () n
lhsToExp (FieldLhs _ fa) = FieldAccess () fa
lhsToExp (ArrayLhs _ ai) = ArrayAccess () ai

-- lockExpToExp is irrelevant - Lock never happens!
lockExpToExp :: Lock PA -> Exp UD
lockExpToExp (Lock _ n _ans) = -- [expQQ| #N#n.isOpen() |]
  MethodInv () (MethodCallOrLockQuery ()
                (Name () MName (Just $ removeAnn n) (Ident () (B.pack "isOpen"))) [])
lockExpToExp (LockVar _ i) =
  MethodInv () (MethodCallOrLockQuery ()
                (Name () MName (Just $ mkSimpleName EName $ removeAnn i)
                          (Ident () (B.pack "isOpen"))) [])

-----
compileLhs :: Lhs PA -> Lhs UD
compileLhs lhs =
    case lhs of
      NameLhs _ n -> NameLhs () (removeAnn n)
      ArrayLhs _ ai -> ArrayLhs () $ compileArrayIndex ai
      FieldLhs _ fa -> FieldLhs () $ compileFieldAccess fa

compileAOp :: AssignOp UD -> Lhs PA -> Exp PA -> Exp UD
compileAOp aop lhs e
    | Just (t1,_) <- ann lhs, t1 == policyT,
      Just (t2,_) <- ann e,   t2 == policyT,
      aop `elem` [MultA (), AddA ()] =
          mkParagonPolicyAssign aop (compileLhs lhs) (compileExp e)
compileAOp aop lhs e = Assign () (compileLhs lhs) aop (compileExp e)

compileArrayIndex :: ArrayIndex PA -> ArrayIndex UD
compileArrayIndex (ArrayIndex _ eArr eI)
    = ArrayIndex () (compileExp eArr) (compileExp eI)

compileFieldAccess :: FieldAccess PA -> FieldAccess UD
compileFieldAccess fa =
    case fa of
      PrimaryFieldAccess _ e i ->
          PrimaryFieldAccess () (compileExp e) (removeAnn i)
      _ -> removeAnn fa

--------------------------------------------
-- Compiling lock and policy declarations

-- Policies

compilePolicyExp :: PolicyExp PA -> Exp UD
compilePolicyExp (PolicyLit _ cs) = callStatic "Policy" "newPolicy"
                                    (Lit () (String () "") : map clauseToExp cs)
compilePolicyExp (PolicyTypeVar _ i) = ExpName () (mkSimpleName EName $ removeAnn i)
-- PolicyOf may only appear in modifiers, which will have been removed.
compilePolicyExp pe =
    panic (compilerModule ++ ".compilePolicyExp")
              $ prettyPrint pe


-- Clauses and components

clauseToExp :: Clause PA -> Exp UD
clauseToExp (Clause _ _ h body) =
    let vs = nub [ a | Var _ a <- universeBi (clauseHeadToActor $ removeAnn h)
                               ++ universeBi (map removeAnn body) ]
             `zip` [0..] -- Substs
        exps = clauseHeadToExp vs h : map (atomToExp vs) body
     in callStatic "Policy" "newPClause" exps

clauseHeadToActor :: ClauseHead UD -> Actor UD
clauseHeadToActor (ClauseDeclHead _ (ClauseVarDecl _ _ i)) = Var () i
clauseHeadToActor (ClauseVarHead _ a) = a

clauseHeadToExp :: [(Ident UD, Int)] -> ClauseHead PA -> Exp UD
clauseHeadToExp vs (ClauseDeclHead _ (ClauseVarDecl t _ i)) =
    actorToExp vs (Var defaultPos i)   -- <<- DefaultPos is a hack
clauseHeadToExp vs (ClauseVarHead _ a) = actorToExp vs a

headToExp, atomToExp :: [(Ident UD, Int)] -> Atom PA -> Exp UD
headToExp vs (Atom _ _ acts) =
    callStatic "ActorList" "newActorList" (map (actorToExp vs) acts)
atomToExp vs (Atom _ n acts) =
    callStatic "Atom" "newAtom" (ExpName () (removeAnn n): map (actorToExp vs) acts)

actorToExp :: [(Ident UD, Int)] -> Actor PA -> Exp UD
actorToExp _vs (Actor _ (ActorName _ n)) = ExpName () $ removeAnn n
actorToExp _vs (Actor _ (ActorTypeVar _ _rt tv)) = ExpName () (mkSimpleName EName (removeAnn tv))
actorToExp vs (Var _ i) =
    let res = lookup (removeAnn i) vs
        k = case res of
              Just m -> fromIntegral m
              Nothing -> panic (compilerModule ++ ".actorToExp")
                         $ "No such actor variable: " ++ show (i, vs)
     in callStatic "Actor" "newActorVariable" [Lit () $ Int () k]


-- Locks

-- Compile a lock declaration into a (static) Lock declaration
-- plus (static) initialization of its lock properties.
-- Precondition: md is a LockDecl
compileLockDecl :: MemberDecl PA -> [Decl UD]
compileLockDecl md =
    case md of
      LockDecl _ ms i@(Ident _ rawI) pars mLProps ->
              let -- Properties defined in modifiers
                  lmExps = map (lockModToExp i) $ filter isLockMod ms
                  -- Properties defined explicitly
                  lpExps = maybe []
                             (map (lockPropToExp i) . (\(LockProperties _ cs) -> cs))
                             mLProps
                  lockE = callStatic "Lock" "newLock"
                               [Lit () $ String () $ B.unpack rawI, Lit () $ Int () (fromIntegral $ length pars)]
                  lockD = FieldDecl () ([Static (),Final ()] `union` removeParagonMods ms)
                            lockType
                            -- [typeQQ| se.chalmers.paragon.Lock |]
                            [vDecl (removeAnn i) lockE]
              in MemberDecl () lockD :
                        lockExpsToInit i (lmExps ++ lpExps)

      _ -> fail $ "Internal error: compileLockDecl: " ++ show md


-- Initialization code for lock properties
lockExpsToInit :: Ident PA -> [Exp UD] -> [Decl UD]
lockExpsToInit _ [] = []
lockExpsToInit _i es = [InitDecl () True . Block () $
                        map (BlockStmt () . ExpStmt ()) es]

lockPropToExp :: Ident PA -> LClause PA -> Exp UD
lockPropToExp _i@(Ident _ rawI) (LClause _ _ h body) =
    let vs = nub [ a | Var _ a <- universeBi (map removeAnn (h:body)) ] `zip` [0..] -- Substs
        exps = headToExp vs h : map (atomToExp vs) body

    in call [B.unpack rawI,"addClause"] exps
lockPropToExp i _ = panic (compilerModule ++ ".lockPropToExp")
                    $ prettyPrint i


lockModToExp :: Ident PA -> Modifier PA -> Exp UD
lockModToExp (Ident _ rawI) m =
    let mname = prettyPrint m
     in call [B.unpack rawI,mname] []
lockModToExp i _ = panic (compilerModule ++ ".lockModToExp")
                   $ prettyPrint i


isLockMod :: Modifier a -> Bool
isLockMod m = case m of
  Reflexive  _ -> True
  Transitive _ -> True
  Symmetric  _ -> True
  _ -> False

isParagonMod :: Modifier a -> Bool
isParagonMod m = case m of
  Typemethod _ -> True
  Readonly   _ -> True
  Notnull    _ -> True
  Reflexive  _ -> True
  Transitive _ -> True
  Symmetric  _ -> True
  Reads _    _ -> True
  Writes _   _ -> True
  Opens _    _ -> True
  Closes _   _ -> True
  Expects _  _ -> True
  _ -> False

removeParagonMods :: [Modifier a] -> [Modifier UD]
removeParagonMods = map removeAnn . filter (not . isParagonMod)

callStatic :: String -> String -> [Exp UD] -> Exp UD
callStatic typ met args =
    MethodInv () $ MethodCallOrLockQuery ()
                  (Name () MName (Just $ mkPkgTypeName typ) (Ident () (B.pack met))) args

call :: [String] -> [Exp UD] -> Exp UD
call strs args =
    MethodInv () $ MethodCallOrLockQuery () (mkName const MName EName $ map (Ident () . B.pack) strs) args

vDecl :: Ident UD -> Exp UD -> VarDecl UD
vDecl i initz = VarDecl () (VarId () i) (Just $ InitExp () initz)


pkgPrefix :: Name UD
pkgPrefix = mkUniformName const PName $ map (Ident () . B.pack) ["se","chalmers","paragon"]

mkPkgTypeName :: String -> Name UD
mkPkgTypeName str = Name () TName (Just pkgPrefix) (Ident () (B.pack str))

mkPkgType :: String -> Type UD
mkPkgType str = RefType () $ ClassRefType () $ ClassType () (mkPkgTypeName str) []

concreteActorType, policyType, lockType :: Type UD
concreteActorType = mkPkgType "ConcreteActor"
policyType        = mkPkgType "Policy"
lockType          = mkPkgType "Lock"