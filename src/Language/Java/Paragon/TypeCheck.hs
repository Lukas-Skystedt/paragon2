-- | Top module for the type checking phase. It follows name resolution and is
-- followed by policy type evaluation.
module Language.Java.Paragon.TypeCheck where

import Language.Java.Paragon.Interaction
import Language.Java.Paragon.Error
import Language.Java.Paragon.Syntax
import Language.Java.Paragon.Pretty
import Language.Java.Paragon.SourcePos(defaultPos)

import Language.Java.Paragon.TypeCheck.Types
import Language.Java.Paragon.TypeCheck.TcStmt
import Language.Java.Paragon.Decorations.PaDecoration
import Language.Java.Paragon.TypeCheck.Monad.TcCodeM

import Language.Java.Paragon.TypeCheck.NotAppl

import qualified Language.Java.Paragon.PolicyLang as PL

import qualified Data.ByteString.Char8 as B
import qualified Data.Map as Map
import Data.Map (Map)


-- | Base name of a file being type checked.
-- | TODO: check what format the string is in.
type BaseName = String


-- type TypeCheck m ast = ast PA -> m (ast TC)

-- | Top level function in the type checking phase.
typeCheck :: PiPath   -- ^ Paths to pi files
          -> BaseName -- ^ Base name of the file
          -> CompilationUnit PA
          -> BaseM (CompilationUnit TC)
typeCheck piDirs baseName (CompilationUnit _ pkg imps [td])= error "typeCheck placeholder evaluated"
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
    --                  | FieldDecl _ ms (RefType _ rt) vds <- mDecls
    --                  , Final defaultPos `elem` ms
    --                  , vd <- vds
    --                  -- Only final ones can be used in policies
    --            ]


typeCheckInitDecls :: [Decl PA] -> TcDeclM [Decl TC]
typeCheckInitDecls is = error "typeCheckInitDecls: not implemented"

--------------------------------------------------------------------------------
-- Bodies
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
typeCheckMemberDecl     _  md = error "typeCheckMemberDecl: not implemented (last case)" --return $ {-notAppl-} md

typeCheckFieldDecl :: CodeState -> TypeCheck TcDeclM MemberDecl
typeCheckFieldDecl st (FieldDecl _ ms _t vds) = error "typeCheckFieldDecl: not implemented"

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
        
      let ms'  = error "ms not type checked"
          tps' = error "tps not type checked"
          rt'  = error "rt not type checked"
          i'   = error "i not type checked"
          ps'  = error "ps not type checked"
          exs' = error "exs not type checked"
      return $ TcMethodDecl ms' tps' rt' i' ps' exs' mb'


typeCheckConstrDecl :: CodeState -> TypeCheck TcDeclM MemberDecl
typeCheckConstrDecl st (ConstructorDecl _ ms tps ci ps _exs cb) = error "typeCheckConstrDecl: not implemented"


tcMethodBody :: TypeCheck TcCodeM MethodBody
tcMethodBody (MethodBody _ mBlock) =
    TcMethodBody <$> maybe (return Nothing) ((Just <$>) . tcBlock) mBlock
