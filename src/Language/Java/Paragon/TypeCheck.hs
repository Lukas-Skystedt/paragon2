-- | Top module for the type checking phase. It follows name resolution and is
-- followed by policy type evaluation.
module Language.Java.Paragon.TypeCheck where

import Language.Java.Paragon.Interaction
import Language.Java.Paragon.Error
import Language.Java.Paragon.Syntax
import Language.Java.Paragon.Pretty
import Language.Java.Paragon.SourcePos(defaultPos)

import Language.Java.Paragon.TypeCheck.Monad
import Language.Java.Paragon.TypeCheck.TypeMap
import Language.Java.Paragon.TypeCheck.Types
import Language.Java.Paragon.TypeCheck.TcStmt
import Language.Java.Paragon.TypeCheck.TcExp
import Language.Java.Paragon.Decorations.PaDecoration
import Language.Java.Paragon.TypeCheck.Monad.CodeM

import Language.Java.Paragon.TypeCheck.NotAppl
import qualified Language.Java.Paragon.PolicyLang as PL

import qualified Data.ByteString.Char8 as B
import qualified Data.Map as Map
import Data.Map (Map)
import Data.List (intercalate)
import Data.Maybe (fromJust)
import Control.Arrow (second)

-- | Base name of a file being type checked.
-- | TODO: check what format the string is in.
type BaseName = String


-- type TypeCheck m ast = ast PA -> m (ast TC)

-- | Top level function in the type checking phase.
typeCheck :: PiPath   -- ^ Paths to pi files
          -> BaseName -- ^ Base name of the file
          -> CompilationUnit PA
          -> BaseM (CompilationUnit TC)
typeCheck piDirs baseName (CompilationUnit _ pkg imps [td]) = do
  let (fullTd, skoTy) = skolemTypeDecl td
  runPiReader piDirs $ runTcDeclM skoTy $ do
    let mPkgPrefix = fmap (\(PackageDecl _ n) -> n) pkg
    typedTd <- typeCheckTd baseName mPkgPrefix td
    return $ TcCompilationUnit (fmap notAppl pkg)
                               (map notAppl imps)
                               [typedTd]
typeCheck _ _ _ =
    fail $ "\n\n" ++ "Encountered multiple type declarations in the same file"

typeCheckTd :: BaseName -> Maybe (Name PA) -> TypeCheck TcDeclM TypeDecl
typeCheckTd baseName mpkg (ClassTypeDecl     _ cdecl)
    = TcClassTypeDecl <$> typeCheckCd baseName mpkg cdecl
typeCheckTd baseName mpkg (InterfaceTypeDecl _ idecl)
  = error "typeCheckTd: not implemented"

typeCheckCd :: BaseName -> Maybe (Name PA) -> TypeCheck TcDeclM ClassDecl
typeCheckCd baseName mpkg (ClassDecl sp ms i tps mSuper _impls (ClassBody _ decls)) = do
  withErrCtxt (ClassContext (prettyPrint i)) $ do

    -- Check that the class has the same name as the file it is defined in
    check (unIdent i == B.pack baseName) $ mkError (FileClassMismatch $ B.unpack (unIdent i)) sp

    let memberDecls = [ mdecl | MemberDecl _ mdecl  <- decls ]
        inits       = [ idecl | idecl@InitDecl {}   <- decls ]
        supers      = maybe [objectType] (:[]) mSuper



    --typeCheckActorFields memberDecls $ do
    typeCheckTMPolLocks memberDecls $ do
        typeCheckSignatures memberDecls $ do
          --  Declarations from class body
          decls <- do
                let inits' = [] -- TODO
                mDs' <- typeCheckMemberDecls memberDecls
                return (inits' ++ map TcMemberDecl mDs')

          return $ TcClassDecl (map notAppl ms)
                               (notAppl i)
                               (map notAppl tps)
                               (fmap notAppl mSuper)
                               (map notAppl _impls)
                               (TcClassBody decls)


typeCheckCd _ _ _ = panic (typeCheckerBase ++ ".typeCheckCd")
                  "Enum decls not yet supported"


objectType :: ClassType PA
objectType = ClassType defaultPos -- TODO defaultPos
              (mkName_ TName PName $
               map (Ident defaultPos . B.pack) ["java","lang","Object"]) []

typeCheckActorFields :: [MemberDecl PA] -> TcDeclM a -> TcDeclM a
typeCheckActorFields mDecls tcba = do
  error "typeCheckActorFields: not implemented"
    -- Find all possible actors
    -- let acts = [ (ms, rt, vd)
    --               Bara för att   | FieldDecl _ ms (RefType _ rt) vds <- mDecls
    --                  , Final defaultPos `elem` ms
    --                  , vd <- vds
    --                  -- Only final ones can be used in policies
    --            ]


typeCheckInitDecls :: [Decl PA] -> TcDeclM [Decl TC]
typeCheckInitDecls is = error "typeCheckInitDecls: not implemented"


--------------------------------------------------------------------------------
-- Bodies -- TODO: maybe move this comment?
--------------------------------------------------------------------------------

typeCheckMemberDecls :: [MemberDecl PA] -> TcDeclM [MemberDecl TC]
typeCheckMemberDecls ms = do
  st <- setupStartState
  mapM (typeCheckMemberDecl st) ms


typeCheckMemberDecl :: CodeState -> TypeCheck TcDeclM MemberDecl
typeCheckMemberDecl     st fd@FieldDecl{} =
    typeCheckFieldDecl  st fd
typeCheckMemberDecl     st md@MethodDecl{} =
    typeCheckMethodDecl st md
typeCheckMemberDecl     st cd@ConstructorDecl{} =
    typeCheckConstrDecl st cd
typeCheckMemberDecl     st mcd@MemberClassDecl{} =
  typeCheckMemberClass st mcd
typeCheckMemberDecl     st mid@MemberInterfaceDecl{} =
  typeCheckMemberInterface st mid

typeCheckFieldDecl :: CodeState -> TypeCheck TcDeclM MemberDecl
typeCheckFieldDecl st (FieldDecl _ ms t vds) = do
  vds' <- mapM (typeCheckVarDecl st) vds
  let ms' = map notAppl ms
      t'  = notAppl t
  return $ TcFieldDecl ms' t' vds'

typeCheckVarDecl :: CodeState -> TypeCheck TcDeclM VarDecl
typeCheckVarDecl st vd@(VarDecl _ (VarId _ i) mInit) = do
  withErrCtxt (FallbackContext ("When checking initializer of field " ++ prettyPrint i)) $ do
--    detailPrint $ "field lookup: "++ B.unpack (unIdent i)
--    detailPrint $ "  when checking: "++prettyPrint vd
    Just (VSig fieldTy fieldPol _ fieldStatic _ _) <- Map.lookup (unIdent i) . fields <$> getTypeMap
    -- Cases for initialization. Either no init, an expression or an array.
    case mInit of
      Nothing -> return $ notAppl vd
      Just (InitExp _ e) -> do
        (e',cs) <- runCodeM (simpleEnv (error "policy missing in typeCheckVarDecl") -- TODO handle the policy stuff
          False  -- Not compile time
          ("field initializer " ++ prettyPrint e) fieldStatic) st $ do
                     (rhsTy, e') <- tcExp e -- TODO: Can we get rid of TcStateType?
                     --debugPrint $ "Trying to assign " ++ prettyPrint rhsTy ++ " to " ++ prettyPrint fieldTy
                     mps <- (unStateType rhsTy) `isAssignableToTc` fieldTy -- TODO: unStateType is temporary
                      -- TODO: is there a function for this? (MonadFail m => Bool -> String -> m ())
                     if mps then return e' else fail "typeCheckVarDecl: type mismatch"

        return $ TcVarDecl (TcVarId $ notAppl i) $ Just $ TcInitExp e'

{-
check :: MonadBase m => Bool -> Error -> m ()
-- check b err = if b then return () else failE err
check = checkC

-- | Fail (but allow continuation?) if predicate does not hold
checkC :: MonadBase m => Bool -> Error -> m ()
checkC b err = if b then return () else failEC () err
-}

      Just (InitArray _ _arr) -> error "typeCheckVarDecl: InitArray not implemented"

typeCheckMethodDecl :: CodeState -> TypeCheck TcDeclM MemberDecl
typeCheckMethodDecl st (MethodDecl _ ms tps rt i ps exs mb) = do
  withErrCtxt (MethodContext (prettyPrint i)) $ do
    withFoldMap withTypeParam tps $ do

      let env = CodeEnv
                { vars = [emptyVarMap] -- TODO
                , lockstate = PL.LockSet [] -- TODO
                , returnI = Nothing -- TODO
                , exnsE = Map.empty -- TODO
                , branchPCE = (Map.empty, []) -- TODO
                , parBounds = [] -- TODO
                , compileTime = False
                , staticContext = isMethodStatic ms
                }
      -- This little thing is what actually checks the body
      ((mb', endSt),cs) <- runCodeM env st $ do
        mb' <- tcMethodBody mb
        endSt <- getState
        return (mb', endSt)

      let ms'  = map notAppl ms
          tps' = map notAppl tps
          rt'  = notAppl rt
          i'   = notAppl i
          ps'  = map notAppl ps
          exs' = map notAppl exs
      return $ TcMethodDecl ms' tps' rt' i' ps' exs' mb'


typeCheckConstrDecl :: CodeState -> TypeCheck TcDeclM MemberDecl
typeCheckConstrDecl st (ConstructorDecl _ ms tps ci ps _exs cb) = error "typeCheckConstrDecl: not implemented"


typeCheckMemberClass :: CodeState -> TypeCheck TcDeclM MemberDecl
typeCheckMemberClass = error "typeCheckMemberClass: not implemented"


typeCheckMemberInterface :: CodeState -> TypeCheck TcDeclM MemberDecl
typeCheckMemberInterface = error "typeCheckMemberInterface: not implemented"

tcMethodBody :: TypeCheck CodeM MethodBody
tcMethodBody (MethodBody _ mBlock) =
    TcMethodBody <$> maybe (return Nothing) ((Just <$>) . tcBlock) mBlock

--------------------------------------------------------------------------------
-- Signatures
--------------------------------------------------------------------------------
typeCheckSignatures :: [MemberDecl PA] -> TcDeclM a -> TcDeclM a
typeCheckSignatures mds declm = do
  debugPrint "Entering typeCheckSignatures..."
  st <- setupStartState
  foldr (typeCheckSignature st) declm mds
   -- $ do debugPrint "Done with typeCheckSignatures"

typeCheckSignature :: CodeState -> MemberDecl PA -> TcDeclM a -> TcDeclM a
-- Fields
typeCheckSignature st _fd@(FieldDecl _ ms t vds) tcba
    | t /= PrimType defaultPos (PolicyT defaultPos) = do

  -- Field identifiers
  let fis = [ i | VarDecl _ (VarId _ i) _ <- vds ]

  -- TODO: what is vti
  vti <- withErrCtxt (FallbackContext ("When checking signature for fields "
                      ++ intercalate ", " (map prettyPrint fis))) $ do
    -- 1. Check field type
    ty <- evalSrcTypeTc t
    
    -- 2. Typecheck and evaluate field policy
    -- TODO: check that no write policy is given
    check (null [ () | Writes{} <- ms ]) $ toUndef
              "Write policies are not allowed on fields."
    let rPolExps = [ e | Reads _ e <- ms ]
    check (length rPolExps <= 1) $ toUndef
              "At most one read modifier allowed per field"
    mapM_ (typeCheckPolicyMod st) rPolExps

    -- 3. Add signature to typemap
    return $ VSig {
                 varType = ty,
                 varPol  = Nothing, -- error "varPol should not be used in TypeCheck phase",
                 varParam  = False,
                 varStatic = isStatic ms,
                 varFinal  = isFinal ms,
                 varNotnull = isNotnull ms
                 }
  withFoldMap (addField vti) vds tcba

    where addField :: VarFieldSig -> VarDecl PA -> TcDeclM a -> TcDeclM a
          addField vti (VarDecl sp (VarId _ i) _) =
              withCurrentTypeMap $ \tm ->
                let iName = unIdent i
                    tmFields = fields tm
                in if Map.notMember iName tmFields
                   then return $ tm { fields = Map.insert iName vti tmFields }
                   else failEither $ mkError (FieldAlreadyDefined (B.unpack iName)) sp
          -- TODO Array stuff depricated, remove?
          addField _ vd = \_ -> fail $ "Deprecated declaration: " ++ prettyPrint vd

-- Methods
typeCheckSignature st (MethodDecl sp ms tps retT i ps exns _mb) tcba
    | Typemethod defaultPos `notElem` ms =
        error "typeCheckSignature: case MethodDecl not implemented"

-- Constructors
typeCheckSignature st (ConstructorDecl sp ms tps i ps exns _mb) tcba =
  error "typeCheckSignature: case ConstructorDecl not implemented"

-- TODO: what is this case? MemberClassDecl, MemberInterfaceDecl
typeCheckSignature _ _ tcba = tcba

typeCheckPolicyMod :: CodeState -> Policy PA -> TcDeclM (Policy TC)
typeCheckPolicyMod st polExp = do
  -- TODO: What is top doing here?
  tp <- PL.topM
  ((ty, polExp'), cs) <- runCodeM (simpleEnv tp
    True
    ("policy modifier " ++ prettyPrint polExp) False)
    st
    (tcExp polExp)

  check (null cs) $ toUndef "Internal WTF: typeCheckPolicyMod: Constraints in policy exp?!?"
  check (isPolicyType ty) $ toUndef $ "Wrong type for policy expression: " ++ prettyPrint ty
  return polExp'

-------------------------------------------------------------------------------
-- Policies, typemethods and locks
-------------------------------------------------------------------------------

typeCheckTMPolLocks :: [MemberDecl PA] -> TcDeclM a -> TcDeclM a
typeCheckTMPolLocks = withFoldMap typeCheckTMPolLock

typeCheckTMPolLock :: MemberDecl PA -> TcDeclM a -> TcDeclM a
typeCheckTMPolLock md@LockDecl{} tcba = typeCheckLockDecl md tcba
typeCheckTMPolLock md@(MethodDecl _ ms _ _ _ _ _ _) tcba =
   error "typeCheckTMPolLock: case MethodDecl not implemented"
--                  | Typemethod defaultPos `elem` ms = typeCheckTMSig md $ do
--                                                st <- setupStartState
--                                                _ <- typeCheckMethodDecl st md
--                                                addTMBody md tcba
typeCheckTMPolLock md@(FieldDecl _ ms (PrimType _ (PolicyT _)) _) tcba
                   | isFinal ms = do
                     typeCheckPolicyField md tcba
typeCheckTMPolLock _ tcba = tcba

-------------------------------------------------------------------------------
-- Locks
-------------------------------------------------------------------------------

typeCheckLockDecl :: MemberDecl PA -> TcDeclM a -> TcDeclM a
typeCheckLockDecl _ _ =  error "typeCheckLockDecl: not implemented"
--typeCheckLockDecl (LockDecl _ ms i rts mProps) tcba = do
--  lsig <- withErrCtxt (LockSignatureContext (prettyPrint i)) $ do
--    let rPolExps = [ e | Reads _ e <- ms ]
--    check (length rPolExps <= 1) $ toUndef
--              "At most one read modifier allowed per field"
--    mapM_ (typeCheckPolicyMod emptyCodeState) rPolExps
--    pol <- getLockPolicy ms
--    rTys <- mapM evalSrcRefTypeTc rts
--    shortProps <- getLockModProps Nothing i ms
--    prs <- evalSrcLockProps i mProps
--    return $ LSig (PL.VarPolicy pol) rTys (shortProps ++ prs)
--  withCurrentTypeMap (\tm -> return $ tm { locks = Map.insert (unIdent i) lsig (locks tm) })
--    tcba
--
--typeCheckLockDecl md _ = panic (typeCheckerBase ++ ".typeCheckLockDecl") $
--                         "Applied to non-lock decl " ++ show md
--
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------------
-- Policies
-- Precondition: only apply on policies
-- TODO: maybe check precondition
typeCheckPolicyField :: MemberDecl PA -> TcDeclM a -> TcDeclM a
typeCheckPolicyField fd@(FieldDecl _ ms t vds) tcba = do
    --debug "typeCheckPolicyField"
    -- 0. Flatten
    let pols = [ (i, initz) | VarDecl _ (VarId _ i) initz <- vds ]
    vti <- withErrCtxt (FallbackContext ("When checking policy fields "
                   ++ intercalate ", " (map (prettyPrint . fst) pols))) $ do

      -- 1. Check that initializer exists
      check (all ((/= Nothing) . snd) pols) $ toUndef
                 "typeCheckPolicyField: Uninitialized policy"
      -- 2. Check that policy is bottom
      check (null [ () | Reads _ _ <- ms ]) $ toUndef
                 "typeCheckPolicyField: Policy must have policy bottom"
      -- 3. Add signature to environment
      tcty <- evalSrcTypeTc t
      bt <- PL.bottomM
      return $ VSig
                { varType = tcty
                , varPol  = Just bt -- TODO: should we really put bottom here? Or Nothing?
                , varParam = False
                , varStatic = isStatic ms
                , varFinal  = isFinal ms
                , varNotnull = False
                }
    withFoldMap (addField vti) (map fst pols) $ do
      -- 4. Typecheck the field normally
      st <- setupStartState
      _  <- typeCheckFieldDecl st fd
      -- 5. Evaluate the initializers
      withFoldMap (evalAddPolicyInit st) (map (second fromJust) pols)
        tcba
            where
              addField :: VarFieldSig -> Ident PA -> TcDeclM a -> TcDeclM a
              addField vti i =
                      withCurrentTypeMap $ \tm ->
                        let Ident sp iName = i
                            tmFields = fields tm
                        in if Map.notMember iName tmFields
                             then return $ tm { fields = Map.insert iName vti tmFields }
                             else failEither $ mkError (PolicyAlreadyDefined (B.unpack iName)) sp
typeCheckPolicyField fd _ = panic (typeCheckerBase ++ ".typeCheckPolicyField") $
                           "Applied to non-policy decl " ++ show fd

evalAddPolicyInit :: CodeState -> (Ident PA, VarInit PA) -> TcDeclM a -> TcDeclM a
evalAddPolicyInit st (i, InitExp _ eInit) tcba = do
  --debugPrint $ "evalAddInit: " ++ show i
  tp <- PL.topM
 --tcPol <- withErrCtxt (FallbackContext ("When evaluating the initializer of field "
 --                        ++ prettyPrint i)) $ evalPolicy eInit
  ((tyInit, _eInit'),_) <- runCodeM (simpleEnv tp False "policy initializer" False) st $ tcExp eInit
  check (isPolicyType tyInit) $ toUndef $
        "Cannot initialize policy field " ++ prettyPrint i ++
        " with non-policy expression " ++ prettyPrint eInit ++ " of type " ++ prettyPrint tyInit
  withCurrentTypeMap (\tm -> return $ tm
    { policies =
      -- TODO: We don't add anything to the map. This needs to be resolved!
      --Map.insert (unIdent i)
                            --(error "using error value for policy in evalAddPolicyInit")
                            (policies tm) })
    tcba
evalAddPolicyInit _ (i, arrInit) _ =
    fail $ "Cannot initialize policy field " ++ prettyPrint i ++
           " with array " ++ prettyPrint arrInit
-- end policies
------------------------------------------------------------------------------

{-

(ClassBody
  [ MemberDecl
     (FieldDecl [Private ,Static ,Final ]
                (RefType (ClassRefType (ClassType "java.lang.Object" [])))
                [VarDecl (VarId (Ident "highObserver")) Nothing])
  , MemberDecl
     (FieldDecl [Public ,Static ,Final ]
           (PrimType (PolicyT ))
           [VarDecl (VarId (Ident "high"))
                    (Just (InitExp (PolicyExp
                      (PolicyLit [Clause []
                                         (ClauseVarHead
                                            (Actor (ActorName "highObserver"))) []]))))])
  , MemberDecl
     (FieldDecl [Private ,Static ,Reads (ExpName "high")] (PrimType (IntT )) [VarDecl (VarId (Ident "x")) (Just (InitExp (Lit (Int 1))))])
  ,MemberDecl
     (FieldDecl [Private ,Static ,Reads (ExpName "high")] (PrimType (IntT )) [VarDecl (VarId (Ident "y")) (Just (InitExp (ExpName "x")))])]))]
-}
