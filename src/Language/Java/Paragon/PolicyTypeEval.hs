-- | Top module for the policy type evaluation phase. It follows type checking
-- and is followed by lock state evaluation.
module Language.Java.Paragon.PolicyTypeEval where

import Language.Java.Paragon.Error
import Language.Java.Paragon.Pretty
import Language.Java.Paragon.Syntax
import Language.Java.Paragon.ToSP
import Language.Java.Paragon.Interaction

import Language.Java.Paragon.Decorations.PteDecoration

import Language.Java.Paragon.PolicyTypeEval.PteExp
import Language.Java.Paragon.PolicyTypeEval.PteStmt

import Language.Java.Paragon.PolicyLang (ActorPolicy)
import qualified Language.Java.Paragon.PolicyLang as PL

import Language.Java.Paragon.TypeCheck.Types
import Language.Java.Paragon.TypeCheck.TypeMap
import Language.Java.Paragon.TypeCheck.Monad.TcDeclM
import Language.Java.Paragon.TypeCheck.Monad.CodeM
import Language.Java.Paragon.TypeCheck.NotAppl2

import Language.Java.Paragon.SourcePos

import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (intercalate)
import Control.Monad (foldM)
import qualified Data.ByteString.Char8 as B

type EvalPT m a = a TC -> m (a PTE)

-- | Top level function in the policy type evaluation phase.
evalPolicyTypes :: CompilationUnit TC -> TcDeclM (CompilationUnit PTE)
evalPolicyTypes (TcCompilationUnit pkg imps [td]) = do
  evalptTd td
  error "evalPolicyTypes placeholder evaluated"
evalPolicyTypes _ = error "evalPolicyTypes: not implemented"

evalptTd :: EvalPT TcDeclM TypeDecl
evalptTd (TcClassTypeDecl cdecl) = PteClassTypeDecl <$> evalptCd cdecl

evalptId :: EvalPT TcDeclM InterfaceDecl
evalptId (TcInterfaceDecl _ _ _ _ idecl) = error "evalptId: not implemented"

evalptCd :: EvalPT TcDeclM ClassDecl
evalptCd (TcClassDecl ms i tps mSuper impls (TcClassBody decls)) = do

  let memberDecls = [ mdecl | TcMemberDecl mdecl  <- decls ]
      inits       = [ idecl | idecl@TcInitDecl {} <- decls ]
      --supers      = maybe [objectType] (:[]) mSuper

  -- Determine if class has a static initializer effect bound
  staticWPol <- PL.VarPolicy <$> getWritePolicy (map toSP ms)

  evalptSignatures memberDecls $ \constrWPol -> do
     --  Declarations from class body
    decls <- do
          let inits' = [] -- TODO
          mDs' <- evalptMemberDecls staticWPol constrWPol memberDecls
          return (inits' ++ map PteMemberDecl mDs')

    return $ PteClassDecl (map  (error "this sucks!") ms)
                          (     (error "this sucks!") i)
                          (map  (error "this sucks!") tps)
                          (fmap (error "this sucks!") mSuper)
                          (map  (error "this sucks!") impls)
                          (PteClassBody decls)

{-
GHC Generics, Data, Typeable, Template Haskell, Uniplate, Multiplate (etc)


data A
data B
data Foo x = Foo (XFoo x)
type instance Foo A = ()
type instance Foo B = ()

Foo A /= Foo B
-}

evalptMemberDecls :: ActorPolicy
                  -> ActorPolicy
                  -> [MemberDecl TC] -> TcDeclM [MemberDecl PTE]
evalptMemberDecls sLim cLim ms = do
  st <- setupPteStartState
  mapM (evalptMemberDecl sLim cLim st) ms


evalptMemberDecl :: ActorPolicy -> ActorPolicy -> PteCodeState -> EvalPT TcDeclM MemberDecl
evalptMemberDecl sLim cLim st fd@FieldDecl{} =
  evalptFieldDecl sLim cLim st fd
evalptMemberDecl _ _ st md@MethodDecl{} =
  evalptMethodDecl st md
evalptMemberDecl _ _ st cd@ConstructorDecl{} =
  evalptConstrDecl st cd
evalptMemberDecl _ _ st mcd@MemberClassDecl{} =
  evalptMemberClass st mcd
evalptMemberDecl _ _ st mid@MemberInterfaceDecl{} =
  evalptMemberInterface st mid


evalptFieldDecl :: ActorPolicy -> ActorPolicy -> PteCodeState -> EvalPT TcDeclM MemberDecl
evalptFieldDecl sLim cLim st (TcFieldDecl ms t vds) = do
  let lim = if isStatic ms then sLim else cLim
  vds' <- mapM (evalptVarDecl lim st) vds
  return $ PteFieldDecl (map notAppl2 ms) (notAppl2 t) vds'

evalptFieldDecl _ _ _ md = panic (policyTypeEvalBase ++ ".evalpteFieldDecl") $
                              "Applied to non-field decl " ++ show md
                              ++ "\nAnd not caught in previous phase"


evalptMethodDecl :: PteCodeState -> EvalPT TcDeclM MemberDecl
evalptMethodDecl st md = error "evalptMethodDecl: not implemented"

evalptConstrDecl :: PteCodeState -> EvalPT TcDeclM MemberDecl
evalptConstrDecl st cd = error "evalptConstrDecl: not implemented"

evalptMemberClass :: PteCodeState -> EvalPT TcDeclM MemberDecl
evalptMemberClass st mcd = error "evalptMemberClass: not implemented"

evalptMemberInterface :: PteCodeState -> EvalPT TcDeclM MemberDecl
evalptMemberInterface st mid = error "evalptMembeerInterface:  not implemented"

evalptVarDecl :: ActorPolicy -> PteCodeState -> EvalPT TcDeclM VarDecl
evalptVarDecl lim st vd@(TcVarDecl (TcVarId i) mInit) = do
  withErrCtxt (FallbackContext ("When checking initializer of field " ++ prettyPrint i)) $ do
    debugPrint $ "evalptVarDecl: " ++ prettyPrint i ++ " : " ++ maybe "" prettyPrint mInit
    -- TODO: Write evalptExp funtion and use it here
    -- case mInit of
    --   Nothing ->
    --   Just (TcVarInit initExp) -> do e' <- evalptExp initExp ...



------------------------------------------------------------------------------
-- Signatures

evalptSignatures :: [MemberDecl TC] -> (ActorPolicy -> TcDeclM a) -> TcDeclM a
evalptSignatures mds tcbaf = do
  debugPrint "Entering evalptSignatures..."
  st <- setupPteStartState
  withFoldMap (evalptSignature st) mds $ do
    debugPrint "Done with evalptSignatures"
    getConstrPol >>= tcbaf

evalptSignature :: PteCodeState -> MemberDecl TC -> TcDeclM a -> TcDeclM a
-- Fields
evalptSignature st _fd@(TcFieldDecl ms t vds) tcba
  | isNotPolicy t = do
  let fis = [ i | TcVarDecl (TcVarId i) _ <- vds ]
  pol <- withErrCtxt (FallbackContext ("When running policy type evaluation for fields "
                      ++ intercalate ", " (map prettyPrint fis))) $ do

    rPol <- getReadPolicy (map toSP ms)
    -- TODO: which phase do these checks belong to?
    -- check (TcFinal `elem` ms || not (PL.includesThisVP rPol)) $ toUndef $ -- TODO isFinal ms?
    --       prettyPrint (PL.thisP :: PL.PrgPolicy) ++
    --       " may not be used as the policy of non-final fields"

    -- check (not $ typeIncludesThis ty) $ toUndef $
    --       prettyPrint (PL.thisP :: PL.PrgPolicy) ++
    --       " may not be used in the type arguments of a field type"
    debugPrint "evalptSignature"
    mapM_ (\(TcVarDecl (TcVarId i) _) ->  debugPrint $ "Updating signature of "++ B.unpack (unIdent i)) vds
    return $ PL.VarPolicy rPol

  withFoldMap (updateField pol) vds tcba

    where
      updateField :: ActorPolicy -> VarDecl TC -> TcDeclM a -> TcDeclM a
      updateField newPolicy (TcVarDecl (TcVarId i) _) =
              withCurrentTypeMap $ \tm ->
                let iName = unIdent i
                    tmFields = fields tm
                in case Map.lookup iName tmFields of
                      Just vsig ->
                          let newVsig = vsig { varPol = Just newPolicy }
                          in return $ tm { fields = Map.insert iName newVsig tmFields }
                      -- TODO: this is a temporary, as values aren't kept in the typemap from the typecheck phas
                      Nothing ->
                        let newVsig = VSig (PrimType () (PolicyT defaultPos)) Nothing False False False False -- vsig { varPol = Nothing }
                        in return $ tm { fields = Map.insert iName newVsig tmFields } -- fail $ "Could not find a signature for " ++ prettyPrint i ++ " when evaluating policy types. TypeMap="++prettyPrint tm
      isNotPolicy (PrimType _ (PolicyT _)) = False
      isNotPolicy _ = True
--MemberClassDecl, MemberInterfaceDecl
evalptSignature _ _ tcba = tcba

getConstrPol :: TcDeclM ActorPolicy
getConstrPol = do
  mConstrs <- constrs <$> getTypeMap
  let wPols = map cWrites $ Map.elems mConstrs
  bt <- PL.bottomM
  foldM PL.lub bt wPols