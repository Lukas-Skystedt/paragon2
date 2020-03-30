-- {-# LANGUAGE PatternGuards #-}
module Language.Java.Paragon.Compile (compileTransform) where

compileTransform :: a
compileTransform = error "compileTransform is not yet implemented"

-- import Language.Java.Paragon.Syntax
-- import Language.Java.Paragon.Pretty
-- import Language.Java.Paragon.Error()
-- import Language.Java.Paragon.TypeCheck.Types
-- import Language.Java.Paragon.Interaction
-- import Language.Java.Paragon.Decorations.PcsDecoration

-- import Data.Generics.Uniplate.Data
-- import Data.List (nub, union, delete)
-- import Control.Arrow ((***))
-- import Control.Monad (void)

-- import qualified Data.ByteString.Char8 as B

-- compilerModule :: String
-- compilerModule = libraryBase ++ ".Compile"

-- compileTransform :: CompilationUnit T -> CompilationUnit PCS
-- compileTransform (CompilationUnit _ mp is tds) =
--     PcsCompilationUnit (fmap voidAnn mp) (map voidAnn is) $ map compileTypeDecl tds

-- compileTypeDecl :: TypeDecl T -> TypeDecl PCS
-- compileTypeDecl td =
--     case td of
--       ClassTypeDecl     _ cdecl -> PcsClassTypeDecl      $ compileClassDecl     cdecl
--       InterfaceTypeDecl _ idecl -> PcsInterfaceTypeDecl  $ compileInterfaceDecl idecl


-- compileInterfaceDecl :: InterfaceDecl T -> InterfaceDecl PCS
-- compileInterfaceDecl (InterfaceDecl _ ms i tps sups ibody) =
--   let ms' = delete (PcsNative) $ removeParagonMods ms -- No lockstate mods allowed here
--                                                       -- In vanilla Java, native cannot appear on classes
--       sups'  = map compileClassType_ sups
--       (tps', _tpMembers, _tpPars, _tpAsss) = splitTypeParams tps
--   in PcsInterfaceDecl  ms' (voidAnn i) tps' sups' $
--        compileInterfaceBody ibody

-- compileInterfaceBody :: InterfaceBody T -> InterfaceBody PCS
-- compileInterfaceBody (InterfaceBody _ mds) =
--     PcsInterfaceBody  $ map (compileSimpleMemberDecl [] []) mds

-- -- 1. Remove Paragon modifiers
-- -- 2. Transform Paragon type params into ordinary params
-- -- 3. Transform body
-- compileClassDecl :: ClassDecl T -> ClassDecl PCS
-- compileClassDecl (ClassDecl _ ms i tps mSuper impls cbody) =
--   let ms' = delete (PcsNative) $ removeParagonMods ms -- No lockstate mods allowed here
--                                                       -- In vanilla Java, native cannot appear on classes
--       mSuper' = fmap compileClassType_ mSuper
--       impls'  = map compileClassType_ impls
--       (tps', tpMembers, tpPars, tpAsss) = splitTypeParams tps
--   in PcsClassDecl  ms' (voidAnn i) tps' mSuper' impls' $
--        compileClassBody cbody tpMembers tpPars tpAsss
-- compileClassDecl _ = panic (compilerModule ++ ".compileClassDecl")
--                            "Enum not supported"

-- -- Paragon type parameters need to be replaced by runtime counterparts.
-- -- 1. Lock state parameters should be removed completed.
-- -- 2. Actor and Policy parameters need to be around at runtime. Each
-- --    parameter is translated into:
-- --     a) a field of the parameterized class
-- --     b) a parameter to every constructor of the class
-- --     c) an assignment of the parameter b) to the field a)
-- --        at the beginning of every constructor of the class
-- splitTypeParams :: [TypeParam T]
--                 -> ([TypeParam PCS],[MemberDecl PCS],[FormalParam PCS],[BlockStmt PCS])
-- splitTypeParams = go ([],[],[],[]) -- error "compileTypeParams undefined"
--     where
--       go (ttps,fds,fps,as) [] = (reverse ttps, reverse fds, reverse fps, reverse as)
--       go (ttps,fds,fps,as) (tp:tps) =
--           case tp of
--             TypeParam{}      -> go (voidAnn tp:ttps,fds,fps,as) tps -- Retain
--             LockStateParam{} -> go (           ttps,fds,fps,as) tps -- Ignore
--             _ -> let (i,ty) =
--                          case tp of
--                            ActorParam  _ rt iP ->
--                                (voidAnn iP, -- [typeQQ| se.chalmers.paragon.ConcreteActor |]
--                                 PcsRefType  (voidAnn rt))    -- concreteActorType)

--                            PolicyParam _ iP ->
--                                (voidAnn iP, -- [typeQQ| se.chalmers.paragon.Policy        |]
--                                 policyType)
--                            _ -> panic (compilerModule ++ ".splitTypeParams")
--                                 $ show tp
--                      fd = --   [fieldDeclQQ| public final #T#ty #i; |]
--                           PcsFieldDecl  [PcsPublic ,PcsFinal ] ty [PcsVarDecl  (PcsVarId  i) Nothing]
--                      fp = -- [formalParamQQ| final #T#ty #i         |]
--                           PcsFormalParam  [PcsFinal ] ty False (PcsVarId  i)
--                      a  = -- [blockStmtQQ| this.#i = #i;         |]
--                           PcsBlockStmt  (PcsExpStmt  (PcsAssign
--                              (PcsFieldLhs  (PcsPrimaryFieldAccess (PcsThis) i))
--                              (PcsEqualA) (PcsExpName (PcsName EOrLName Nothing i))))
--                  in go (ttps,fd:fds,fp:fps,a:as) tps


-- compileClassBody :: ClassBody T -> [MemberDecl PCS] -> [FormalParam PCS] -> [BlockStmt PCS] -> ClassBody PCS
-- compileClassBody (ClassBody _ ds) tpMembers tpPars tpAsss =
--   let ds' = concatMap (compileDecl tpPars tpAsss) ds
--   in PcsClassBody (map (PcsMemberDecl) tpMembers ++ ds')

-- compileDecl :: [FormalParam PCS] -> [BlockStmt PCS] -> Decl T -> [Decl PCS]
-- compileDecl _ _ InitDecl{} = panic (compilerModule ++ ".compileDecl")
--                                "InitDecl not yet supported"
-- compileDecl tpPars tpAsss (MemberDecl _ md) = compileMemberDecl tpPars tpAsss md

-- compileMemberDecl :: [FormalParam PCS] -> [BlockStmt PCS] -> MemberDecl T -> [Decl PCS]
-- compileMemberDecl tpPars tpAsss md =
--     case md of
--       LockDecl {} -> compileLockDecl md
--       _ -> (:[]) . PcsMemberDecl $ compileSimpleMemberDecl tpPars tpAsss md

-- compileSimpleMemberDecl :: [FormalParam PCS] -> [BlockStmt PCS] -> MemberDecl T -> MemberDecl PCS
-- compileSimpleMemberDecl tpPars tpAsss md =
--     case md of
--       -- Actors
--       FieldDecl _ ms t vds -> compileVarDeclGeneric (PcsFieldDecl) ms t vds

--       MethodDecl _ ms tps rt i fps xs mb ->
--           let ms' = removeParagonMods ms
--               (tps', _, tpPs, _) = splitTypeParams tps
--               rt' = compileReturnType rt
--               fps' = map compileFormalParam fps
--               xs' = map compileExn xs
--           in PcsMethodDecl ms' tps' rt' (voidAnn i) (tpPs ++ fps') xs' $ compileMethodBody mb

--       ConstructorDecl _ ms tps i fps xs cb ->
--           let ms' = removeParagonMods ms
--               (tps', _, tpPs, _) = splitTypeParams tps
--               fps' = map compileFormalParam fps
--               xs'  = map compileExn xs
--               -- Add the downgraded type parameters: class parameters (tpPars) first,
--               -- then parameters specific to this constructor (tpPs)
--           in PcsConstructorDecl ms' tps' (voidAnn i) (tpPars ++ tpPs ++ fps') xs'
--                  $ compileConstrBody tpAsss cb

--       _ -> panic (compilerModule ++ ".compileSimpleMemberDecl")
--            $ prettyPrint md -- Locks should be filtered out already

-- compileConstrBody :: [BlockStmt PCS] -> ConstructorBody T -> ConstructorBody PCS
-- compileConstrBody tpAsss (ConstructorBody _ meci bss) =
--     -- Add the initialization of the downgraded type parameters
--     PcsConstructorBody (fmap compileECI meci) (tpAsss ++ map compileBlockStmt bss)

-- compileECI :: ExplConstrInv T -> ExplConstrInv PCS
-- compileECI (ThisInvoke  _ tas as) =
--     let (trueTas, demotedArgs) = splitNWTypeArgs tas
--     in ThisInvoke  () trueTas (demotedArgs ++ map compileExp as)
-- compileECI (SuperInvoke _ tas as) =
--     let (trueTas, demotedArgs) = splitNWTypeArgs tas
--     in PcsSuperInvoke trueTas (demotedArgs ++ map compileExp as)
-- compileECI (PrimarySuperInvoke _ e tas as) =
--     let (trueTas, demotedArgs) = splitNWTypeArgs tas
--     in PcsPrimarySuperInvoke (compileExp e) trueTas (demotedArgs ++ map compileExp as)

-- compileFormalParam :: FormalParam T -> FormalParam PCS
-- compileFormalParam (FormalParam _ ms t va vid) =
--     PcsFormalParam (removeParagonMods ms) (compileType t) va (voidAnn vid)

-- policyVarDecl, compileVarDecl :: VarDecl T -> VarDecl PCS

-- policyVarDecl (VarDecl _ (VarId _ i@(Ident _ rawI))
--                            (Just (InitExp _ (PolicyExp _ (PolicyLit _ cs)))))
--     = vDecl (voidAnn i) $ callStatic "Policy" "newPolicy"
--       (PcsLit (PcsString $ B.unpack rawI) : map clauseToExp cs)
-- policyVarDecl vd = compileVarDecl vd

-- compileVarDecl (VarDecl _ vid mInit) = PcsVarDecl (voidAnn vid) $ fmap compileVarInit mInit

-- compileExn :: ExceptionSpec T -> ExceptionSpec PCS
-- compileExn (ExceptionSpec _ _ms rt) = PcsExceptionSpec [] -- no modifiers on exceptions in java!
--                                        $ compileRefType rt

-- compileReturnType :: ReturnType T -> ReturnType PCS
-- compileReturnType (LockType _) = PcsType $ PcsPrimType $ PcsBooleanT
-- compileReturnType (Type _ t)   = PcsType $ compileType t
-- compileReturnType rett = voidAnn rett

-- compileType :: Type T -> Type PCS
-- compileType (RefType _ rt) = PcsRefType $ compileRefType rt
-- compileType t@(PrimType _ pt) = case pt of
--                                   ActorT _  -> concreteActorType
--                                   PolicyT _ -> policyType
--                                   _ -> voidAnn t
-- compileType t = voidAnn t

-- compileRefType :: RefType T -> RefType PCS
-- compileRefType (ArrayType _ t mps) =
--     PcsArrayType (compileType t) $ map (const Nothing) mps -- No policy parameters!
-- compileRefType (ClassRefType _ ct) = PcsClassRefType $ compileClassType_ ct
-- compileRefType rt = voidAnn rt

-- compileClassType :: ClassType T -> (ClassType PCS, [Argument PCS])
-- compileClassType (ClassType _ n tas) =
--     let (trueTas, demotedArgs) = splitTypeArgs tas
--     in (PcsClassType (voidAnn n) trueTas, demotedArgs)

-- -- When we don't care about demoted policies and actors
-- compileClassType_ :: ClassType T -> ClassType PCS
-- compileClassType_ = fst . compileClassType

-- --compileTypeArgs :: [TypeArgument T] -> [PcsTypeArgument]
-- --compileTypeArgs _ = [] -- TODO: Cheating!!!

-- --compileNWTypeArgs :: [NonWildTypeArgument T] -> [PcsNonWildTypeArgument]
-- --compileNWTypeArgs _ = [] -- TODO: Cheating!!!

-- compileMethodBody :: MethodBody T -> MethodBody PCS
-- compileMethodBody (MethodBody _ (Just bl)) = PcsMethodBody . Just $ compileBlock bl
-- compileMethodBody mb = voidAnn mb

-- compileBlock :: Block T -> Block PCS
-- compileBlock (Block _ bss) = PcsBlock $ map compileBlockStmt bss

-- compileBlockStmt :: BlockStmt T -> BlockStmt PCS
-- compileBlockStmt (BlockStmt _ stmt) = PcsBlockStmt $ compileStmt stmt
-- compileBlockStmt (LocalVars _ ms t vds) = compileVarDeclGeneric (PcsLocalVars) ms t vds
-- compileBlockStmt bss = panic (compilerModule ++ ".compileBlockStmt")
--                        $ prettyPrint bss

-- compileVarDeclGeneric :: ([Modifier PCS] -> Type PCS -> [VarDecl PCS] -> res)
--                        -> [Modifier T ] -> Type T  -> [VarDecl T ] -> res
-- compileVarDeclGeneric con ms t vds =
--     let (t', vds') = case t of
--                        PrimType _ (PolicyT _) -> (policyType, map policyVarDecl vds)
-- --                       PrimType _ (ActorT  _) -> (concreteActorType, map actorVarDecl vds)
--                        _ -> (compileType t, map compileVarDecl vds)
--     in con (removeParagonMods ms)  t' vds'



-- compileStmt :: Stmt T -> Stmt PCS
-- compileStmt (StmtBlock _ bl) = PcsStmtBlock $ compileBlock bl
-- compileStmt (Open t (Lock _ lN aN )) = PcsExpStmt $ PcsMethodInv $ PcsMethodCallOrLockQuery
--          (PcsName MName (Just $ voidAnn lN)
--                    (PcsIdent (B.pack "open")))
--          $ map (compileExp . (\aname -> case aname of
--                                           ActorName _ x -> ExpName t x
--                                           ActorTypeVar _ _ i -> ExpName t (Name t EName Nothing i)
--                              )) aN
-- compileStmt (Close t (Lock _ lN aN )) = PcsExpStmt $ PcsMethodInv $ PcsMethodCallOrLockQuery
--          (PcsName MName (Just $ voidAnn lN)
--                    (PcsIdent (B.pack "close")))
--          $ map (compileExp . (\(ActorName _ x) -> ExpName t x)) aN
-- compileStmt (OpenBlock  _ _ bl) = PcsStmtBlock $ compileBlock bl
-- compileStmt (CloseBlock _ _ bl) = PcsStmtBlock $ compileBlock bl

-- compileStmt (IfThen _ e s) = PcsIfThen (compileExp e) (compileStmt s)
-- compileStmt (IfThenElse _ e th el) = PcsIfThenElse (compileExp e) (compileStmt th) (compileStmt el)
-- compileStmt (While _ e s) = PcsWhile (compileExp e) (compileStmt s)
-- compileStmt (BasicFor _ mIn mTest mUp s) =
--     let mIn' = fmap compileForInit mIn
--         mTest' = fmap compileExp mTest
--         mUp' = fmap (map compileExp) mUp
--     in PcsBasicFor mIn' mTest' mUp' $ compileStmt s
-- compileStmt (ExpStmt _ e) = PcsExpStmt $ compileExp e
-- compileStmt (Return _ me) = PcsReturn $ fmap compileExp me
-- compileStmt (Throw _ e) = PcsThrow $ compileExp e
-- compileStmt (Try _ bl cs mfin) = PcsTry (compileBlock bl) (map compileCatch cs) (fmap compileBlock mfin)
-- compileStmt st = voidAnn st

-- compileForInit :: ForInit T -> ForInit PCS
-- compileForInit (ForInitExps _ es) = PcsForInitExps $ map compileExp es
-- compileForInit (ForLocalVars _ ms t vds) = compileVarDeclGeneric (PcsForLocalVars) ms t vds

-- compileCatch :: Catch T -> Catch PCS
-- compileCatch (Catch _ fp bl) = PcsCatch (compileFormalParam fp) (compileBlock bl)

-- compileVarInit :: VarInit T -> VarInit PCS
-- compileVarInit (InitExp   _ e ) = InitExp   () $ compileExp e
-- compileVarInit (InitArray _ ai) = PcsInitArray $ compileArrayInit ai

-- compileArrayInit :: ArrayInit T -> ArrayInit PCS
-- compileArrayInit (ArrayInit _ vis) = PcsArrayInit $ map compileVarInit vis

-- --compileExp :: Exp T -> PcsExp
-- --compileExp = transformBi compileExp'

-- -- TODO: Fill out full, since no longer generic
-- compileExp :: Exp T -> Exp PCS
-- compileExp (PolicyExp _ pe) = compilePolicyExp pe

-- -- For instance creation, we need to move type
-- -- arguments to actual arguments -- but not right now!
-- compileExp (InstanceCreation a tas ct args mcbody) =
--     let isNative = maybe False snd a
--         (trueTas, demotedArgs) = splitTypeArgs tas
--         (ct', classDemotedArgs) = compileClassType ct
--         extraArgs = if isNative then [] else demotedArgs ++ classDemotedArgs
--     -- Here we need to demote the args in the ct as well!!!
--     in PcsInstanceCreation trueTas ct' (extraArgs ++ map compileExp args)
--                          (fmap (\cb -> compileClassBody cb [] [] []) mcbody)
-- {-
-- compileExp (QualInstanceCreation e tas i args mcbody) = do
--   undefined -}
-- compileExp (ArrayCreate _ t edims idims) =
--     let edims' = map (compileExp *** const Nothing) edims
--         idims' = map (const Nothing) idims
--     in PcsArrayCreate (compileType t) edims' idims'
-- compileExp e@ArrayCreateInit{} =
--     error $ "Compilation of ArrayCreateInit not yet supported: " ++ prettyPrint e

-- compileExp (FieldAccess _ fa) = PcsFieldAccess $ compileFieldAccess fa
-- compileExp (MethodInv _ mi) = PcsMethodInv $ compileMethodInv mi
-- compileExp (ArrayAccess _ ai) = PcsArrayAccess $ compileArrayIndex ai

-- compileExp (PostIncrement _ e) =  PcsPostIncrement $ compileExp e
-- compileExp (PreIncrement  _ e) =  PreIncrement  () $ compileExp e
-- compileExp (PostDecrement _ e) =  PcsPostDecrement $ compileExp e
-- compileExp (PreDecrement  _ e) =  PreDecrement  () $ compileExp e

-- compileExp (PrePlus     _ e) =  PrePlus     () $ compileExp e
-- compileExp (PreMinus    _ e) =  PreMinus    () $ compileExp e
-- compileExp (PreBitCompl _ e) =  PcsPreBitCompl $ compileExp e
-- compileExp (PreNot      _ e) =  PreNot      () $ compileExp e

-- compileExp (Cast _ t e) = PcsCast (compileType t) (compileExp e)

-- -- Lock names must be handled in a special way - but not right now!
-- compileExp (ExpName _ n) = PcsExpName $ voidAnn n
-- compileExp (LockExp _ l) = lockExpToExp l

-- -- Certain operators have special effects on paragon types
-- compileExp (BinOp _ e1 op e2) = compileBinOp (voidAnn op) e1 e2

-- compileExp (ClassLit _ mt) = PcsClassLit (fmap compileType mt)
-- compileExp (Paren _ e) = PcsParen $ compileExp e
-- compileExp (Cond _ c th el) = PcsCond (compileExp c) (compileExp th) (compileExp el)

-- compileExp (Assign _ lhs aop e) = compileAOp (voidAnn aop) lhs e
-- --    PcsAssign (compileLhs lhs) (compileAOp aop) (compileExp e)

-- compileExp e@InstanceOf{} =
--     error $ "Compilation of InstanceOf not yet supported: " ++ prettyPrint e

-- -- Lit, This, ThisClass, AntiQExp
-- compileExp e = voidAnn e


-- compileMethodInv :: MethodInvocation T -> MethodInvocation PCS
-- compileMethodInv mi = case mi of
--   MethodCallOrLockQuery _ n@(Name _ LName _ _) as ->
--       PcsMethodCallOrLockQuery
--          (PcsName MName (Just $ voidAnn n)
--                    (PcsIdent (B.pack "isOpen")))
--          $ map compileExp as

--   MethodCallOrLockQuery _ n as -> PcsMethodCallOrLockQuery (voidAnn n) $ map compileExp as
--   PrimaryMethodCall _ e tas i as ->
--       let (trueTas,demotedArgs) = splitNWTypeArgs tas
--           args = (if isNative then id else (demotedArgs ++)) $ map compileExp as
--        in PcsPrimaryMethodCall (compileExp e) trueTas (voidAnn i) args
--   TypeMethodCall _ n tas i as ->
--       let (trueTas,demotedArgs) = splitNWTypeArgs tas
--           args = (if isNative then id else (demotedArgs ++)) $ map compileExp as
--        in PcsTypeMethodCall (voidAnn n) trueTas (voidAnn i) args
--   _ -> panic (compilerModule ++ ".compileMethodInv")
--        $ prettyPrint mi
--   where isNative = maybe False snd $ ann mi

-- splitTypeArgs :: [TypeArgument T] -> ([TypeArgument PCS], [Argument PCS])
-- splitTypeArgs = go ([], [])
--     where
--       go (ttas, as) [] = (reverse ttas, reverse as)
--       go (ttas, as) (ta:tas) =
--           case ta of
--             Wildcard{} -> panic (compilerModule ++ ".splitTypeArgs")
--                           "Wildcards not yet supported"
--                           -- go (compileWildcard ta:ttas, as) tas
--             ActualArg _ nwta ->
--                 case nwta of
--                   ActualType _ rt ->
--                       let ta' = PcsActualArg $ PcsActualType $ compileRefType rt
--                       in go (ta':ttas, as) tas
--                   ActualName _ (Name _ TName _ _) -> go (voidAnn ta:ttas, as) tas
--                   ActualName _ n@(Name _ EName _ _) -> go (ttas, PcsExpName (voidAnn n) : as) tas
--                   ActualExp _ e -> go (ttas, compileExp e : as) tas
--                   -- Lock states and lock names have no runtime counterpart, so we just ignore those
--                   _ -> go (ttas, as) tas

-- splitNWTypeArgs :: [NonWildTypeArgument T] -> ([NonWildTypeArgument PCS], [Argument PCS])
-- splitNWTypeArgs = go ([], [])
--     where
--       go (ttas, as) [] = (reverse ttas, reverse as)
--       go (ttas, as) (nwta:tas) =
--                 case nwta of
--                   ActualType{} -> go (voidAnn nwta:ttas, as) tas
--                   ActualName _ (Name _ TName _ _) -> go (voidAnn nwta:ttas, as) tas
--                   ActualName _ n@(Name _ EName _ _) -> go (ttas, PcsExpName (voidAnn n) : as) tas
--                   ActualExp _ e -> go (ttas, compileExp e : as) tas
--                   -- Lock states and lock names have no runtime counterpart, so we just ignore those
--                   _ -> go (ttas, as) tas

-- -- Compiling binary operators. The interesting cases are
-- -- the ones where the operands are policies.
-- compileBinOp :: Op PCS -> Exp T -> Exp T -> Exp PCS
-- compileBinOp op e1 e2
--              | Just (t1, _) <- ann e1, t1 == policyT,
--                Just (t2, _) <- ann e2, t2 == policyT,
--                op `elem` [PcsMult, PcsAdd] =
--                    mkParagonPolicyOp op (compileExp e1) (compileExp e2)
-- compileBinOp op e1 e2 = PcsBinOp (compileExp e1) op (compileExp e2)

-- mkParagonPolicyOp :: Op PCS -> Exp PCS -> Exp PCS -> Exp PCS
-- mkParagonPolicyOp op e1 e2 =
--     case op of
--       Mult _ -> PcsMethodInv $
--                  PcsPrimaryMethodCall e1 [] (PcsIdent (B.pack "join")) [e2]
--       Add  _ -> PcsMethodInv $
--                  PcsPrimaryMethodCall e1 [] (PcsIdent (B.pack "meet")) [e2]
--       _ -> panic (compilerModule ++ ".mkParagonPolicyOp")
--            $ "Unexpected operator: " ++ show op

-- mkParagonPolicyAssign :: AssignOp PCS -> Lhs PCS -> Exp PCS -> Exp PCS
-- mkParagonPolicyAssign aop lhs e =
--     case aop of
--       MultA _ -> PcsMethodInv $
--                   PcsPrimaryMethodCall (lhsToExp lhs) []
--                    (PcsIdent (B.pack "joinWith")) [e]
--       AddA  _ -> PcsMethodInv $
--                   PcsPrimaryMethodCall (lhsToExp lhs) []
--                    (PcsIdent (B.pack "meetWith")) [e]
--       _ -> PcsAssign lhs aop e

-- lhsToExp :: Lhs PCS -> Exp PCS
-- lhsToExp (NameLhs _ n) = PcsExpName n
-- lhsToExp (FieldLhs _ fa) = PcsFieldAccess fa
-- lhsToExp (ArrayLhs _ ai) = PcsArrayAccess ai

-- -- lockExpToExp is irrelevant - Lock never happens!
-- lockExpToExp :: Lock T -> Exp PCS
-- lockExpToExp (Lock _ n _ans) = -- [expQQ| #N#n.isOpen() |]
--   PcsMethodInv (PcsMethodCallOrLockQuery
--                 (PcsName MName (Just $ voidAnn n) (PcsIdent (B.pack "isOpen"))) [])
-- lockExpToExp (LockVar _ i) =
--   PcsMethodInv (PcsMethodCallOrLockQuery
--                 (PcsName MName (Just $ mkSimpleName EName $ voidAnn i)
--                           (PcsIdent (B.pack "isOpen"))) [])

-- -----
-- compileLhs :: Lhs T -> Lhs PCS
-- compileLhs lhs =
--     case lhs of
--       NameLhs _ n -> PcsNameLhs (voidAnn n)
--       ArrayLhs _ ai -> PcsArrayLhs $ compileArrayIndex ai
--       FieldLhs _ fa -> PcsFieldLhs $ compileFieldAccess fa

-- compileAOp :: AssignOp PCS -> Lhs T -> Exp T -> Exp PCS
-- compileAOp aop lhs e
--     | Just (t1,_) <- ann lhs, t1 == policyT,
--       Just (t2,_) <- ann e,   t2 == policyT,
--       aop `elem` [PcsMultA, PcsAddA] =
--           mkParagonPolicyAssign aop (compileLhs lhs) (compileExp e)
-- compileAOp aop lhs e = PcsAssign (compileLhs lhs) aop (compileExp e)

-- compileArrayIndex :: ArrayIndex T -> ArrayIndex PCS
-- compileArrayIndex (ArrayIndex _ eArr eI)
--     = PcsArrayIndex (compileExp eArr) (compileExp eI)

-- compileFieldAccess :: FieldAccess T -> FieldAccess PCS
-- compileFieldAccess fa =
--     case fa of
--       PrimaryFieldAccess _ e i ->
--           PcsPrimaryFieldAccess (compileExp e) (voidAnn i)
--       _ -> voidAnn fa

-- --------------------------------------------
-- -- Compiling lock and policy declarations

-- -- Policies

-- compilePolicyExp :: PolicyExp T -> Exp PCS
-- compilePolicyExp (PolicyLit _ cs) = callStatic "Policy" "newPolicy"
--                                     (PcsLit (PcsString "") : map clauseToExp cs)
-- compilePolicyExp (PolicyTypeVar _ i) = PcsExpName (mkSimpleName EName $ voidAnn i)
-- -- PolicyOf may only appear in modifiers, which will have been removed.
-- compilePolicyExp pe =
--     panic (compilerModule ++ ".compilePolicyExp")
--               $ prettyPrint pe


-- -- Clauses and components

-- clauseToExp :: Clause T -> Exp PCS
-- clauseToExp (Clause _ _ h body) =
--     let vs = nub [ a | Var _ a <- universeBi (clauseHeadToActor $ voidAnn h)
--                                ++ universeBi (map voidAnn body) ]
--              `zip` [0..] -- Substs
--         exps = clauseHeadToExp vs h : map (atomToExp vs) body
--      in callStatic "Policy" "newPClause" exps

-- clauseHeadToActor :: ClauseHead PCS -> Actor PCS
-- clauseHeadToActor (ClauseDeclHead _ (ClauseVarDecl _ _ i)) = PcsVar i
-- clauseHeadToActor (ClauseVarHead _ a) = a

-- clauseHeadToExp :: [(Ident PCS, Int)] -> ClauseHead T -> Exp PCS
-- clauseHeadToExp vs (ClauseDeclHead _ (ClauseVarDecl _ _ i)) =
--     actorToExp vs (Var Nothing i)
-- clauseHeadToExp vs (ClauseVarHead _ a) = actorToExp vs a

-- headToExp, atomToExp :: [(Ident PCS, Int)] -> Atom T -> Exp PCS
-- headToExp vs (Atom _ _ acts) =
--     callStatic "ActorList" "newActorList" (map (actorToExp vs) acts)
-- atomToExp vs (Atom _ n acts) =
--     callStatic "Atom" "newAtom" (PcsExpName (voidAnn n): map (actorToExp vs) acts)

-- actorToExp :: [(Ident PCS, Int)] -> Actor T -> Exp PCS
-- actorToExp _vs (Actor _ (ActorName _ n)) = PcsExpName $ voidAnn n
-- actorToExp _vs (Actor _ (ActorTypeVar _ _rt tv)) = PcsExpName (mkSimpleName EName (voidAnn tv))
-- actorToExp vs (Var _ i) =
--     let res = lookup (voidAnn i) vs
--         k = case res of
--               Just m -> fromIntegral m
--               Nothing -> panic (compilerModule ++ ".actorToExp")
--                          $ "No such actor variable: " ++ show (i, vs)
--      in callStatic "Actor" "newActorVariable" [PcsLit $ PcsInt k]


-- -- Locks

-- -- Compile a lock declaration into a (static) Lock declaration
-- -- plus (static) initialization of its lock properties.
-- -- Precondition: md is a LockDecl
-- compileLockDecl :: MemberDecl T -> [Decl PCS]
-- compileLockDecl md =
--     case md of
--       LockDecl _ ms i@(Ident _ rawI) pars mLProps ->
--               let -- Properties defined in modifiers
--                   lmExps = map (lockModToExp i) $ filter isLockMod ms
--                   -- Properties defined explicitly
--                   lpExps = maybe []
--                              (map (lockPropToExp i) . (\(LockProperties _ cs) -> cs))
--                              mLProps
--                   lockE = callStatic "Lock" "newLock"
--                                [PcsLit $ PcsString $ B.unpack rawI, PcsLit $ PcsInt (fromIntegral $ length pars)]
--                   lockD = PcsFieldDecl ([PcsStatic,PcsFinal] `union` removeParagonMods ms)
--                             lockType
--                             -- [typeQQ| se.chalmers.paragon.Lock |]
--                             [vDecl (voidAnn i) lockE]
--               in PcsMemberDecl lockD :
--                         lockExpsToInit i (lmExps ++ lpExps)

--       _ -> fail $ "Internal error: compileLockDecl: " ++ show md


-- -- Initialization code for lock properties
-- lockExpsToInit :: Ident T -> [Exp PCS] -> [Decl PCS]
-- lockExpsToInit _ [] = []
-- lockExpsToInit _i es = [PcsInitDecl True . PcsBlock $
--                         map (PcsBlockStmt . PcsExpStmt) es]

-- lockPropToExp :: Ident T -> LClause T -> Exp PCS
-- lockPropToExp _i@(Ident _ rawI) (LClause _ _ h body) =
--     let vs = nub [ a | Var _ a <- universeBi (map voidAnn (h:body)) ] `zip` [0..] -- Substs
--         exps = headToExp vs h : map (atomToExp vs) body

--     in call [B.unpack rawI,"addClause"] exps
-- lockPropToExp i _ = panic (compilerModule ++ ".lockPropToExp")
--                     $ prettyPrint i

-- lockModToExp :: Ident T -> Modifier T -> Exp PCS
-- lockModToExp (Ident _ rawI) m =
--     let mname = prettyPrint m
--      in call [B.unpack rawI,mname] []
-- lockModToExp i _ = panic (compilerModule ++ ".lockModToExp")
--                    $ prettyPrint i


-- isLockMod :: Modifier a -> Bool
-- isLockMod m = case m of
--   Reflexive  _ -> True
--   Transitive _ -> True
--   Symmetric  _ -> True
--   _ -> False

-- isParagonMod :: Modifier a -> Bool
-- isParagonMod m = case m of
--   Typemethod _ -> True
--   Readonly   _ -> True
--   Notnull    _ -> True
--   Reflexive  _ -> True
--   Transitive _ -> True
--   Symmetric  _ -> True
--   Reads _    _ -> True
--   Writes _   _ -> True
--   Opens _    _ -> True
--   Closes _   _ -> True
--   Expects _  _ -> True
--   _ -> False

-- removeParagonMods :: [Modifier a] -> [Modifier PCS]
-- removeParagonMods = map voidAnn . filter (not . isParagonMod)

-- callStatic :: String -> String -> [Exp PCS] -> Exp PCS
-- callStatic typ met args =
--     PcsMethodInv $ PcsMethodCallOrLockQuery
--                   (PcsName MName (Just $ mkPkgTypeName typ) (PcsIdent (B.pack met))) args

-- call :: [String] -> [Exp PCS] -> Exp PCS
-- call strs args =
--     PcsMethodInv $ PcsMethodCallOrLockQuery (mkName const MName EName $ map (PcsIdent . B.pack) strs) args

-- vDecl :: Ident PCS -> Exp PCS -> VarDecl PCS
-- vDecl i initz = PcsVarDecl (PcsVarId i) (Just $ PcsInitExp initz)


-- pkgPrefix :: Name PCS
-- pkgPrefix = mkUniformName const PName $ map (PcsIdent . B.pack) ["se","chalmers","paragon"]

-- mkPkgTypeName :: String -> Name PCS
-- mkPkgTypeName str = PcsName TName (Just pkgPrefix) (PcsIdent (B.pack str))

-- mkPkgType :: String -> Type PCS
-- mkPkgType str = PcsRefType $ PcsClassRefType $ PcsClassType (mkPkgTypeName str) []

-- concreteActorType, policyType, lockType :: Type PCS
-- concreteActorType = mkPkgType "ConcreteActor"
-- policyType        = mkPkgType "Policy"
-- lockType          = mkPkgType "Lock"

-- voidAnn :: ast a -> ast PCS
-- -- voidAnn = void
-- voidAnn = error "voidAnn not yet implemented"
