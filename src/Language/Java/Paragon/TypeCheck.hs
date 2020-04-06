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
import Language.Java.Paragon.TypeCheck.Monad.TcCodeM

import Language.Java.Paragon.TypeCheck.NotAppl
import qualified Language.Java.Paragon.PolicyLang as PL

import qualified Data.ByteString.Char8 as B
import qualified Data.Map as Map
import Data.Map (Map)
import Data.List (intercalate)

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
    tm <- getTypeMap
    detailPrint $ prettyPrint $ fields tm
    Just (VSig fieldTy fieldPol _ fieldStatic _ _) <- Map.lookup (unIdent i) . fields <$> getTypeMap
    -- Cases for initialization. Either no init, an expression or an array.
    case mInit of
      Nothing -> error "typeCheckVarDecl: notAppl not implemented for varDecl" -- return $ notAppl vd
      Just (InitExp _ e) -> do
        (e',cs) <- runTcCodeM (simpleEnv (error "policy missing in typeCheckVarDecl") -- TODO handle the policy stuff
          False  -- Not compile time
          ("field initializer " ++ prettyPrint e) fieldStatic) st $ do
                     (rhsTy, e') <- tcExp e -- TODO: Can we get rid of TcStateType?
                     mps <- fieldTy `isAssignableToTc` (unStateType rhsTy) -- TODO: unStateType is temporary
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
      ((mb', endSt),cs) <- runTcCodeM env st $ do
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

tcMethodBody :: TypeCheck TcCodeM MethodBody
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

    -- TODO: we use top as a placeholder value since VSig needs one.
    top <- PL.topM
    -- 2. Add signature to typemap
    return $ VSig {
                 varType = ty,
                 varPol  = top, -- error "varPol should not be used in TypeCheck phase",
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

--Use this instead
-- TODO: We replace the old evalSrcType with this function.
-- TODO: Maybe 'TcDeclM' should be replaced with a generic monad (EvalPolicyM).
evalSrcTypeTc :: Type PA -> TcDeclM (Type TC)
evalSrcTypeTc (PrimType _ pt) = return $ TcPrimType (primTypePaToTc pt)
evalSrcTypeTc _ = error "evalSrcTypeTc: not implemented"