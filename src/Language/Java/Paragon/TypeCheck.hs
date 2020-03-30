-- | Top module for the type checking phase. It follows name resolution and is
-- followed by policy type evaluation.
module Language.Java.Paragon.TypeCheck where

import Language.Java.Paragon.Interaction
import Language.Java.Paragon.Error
import Language.Java.Paragon.Syntax
import Language.Java.Paragon.Pretty

import Language.Java.Paragon.TypeCheck.Types
import Language.Java.Paragon.Decorations.PaDecoration
import Language.Java.Paragon.TypeCheck.Monad.TcCodeM

import qualified Data.ByteString.Char8 as B


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

    return $ ClassBody Nothing <$> do
      let inits' = [] -- TODO 
          mDs' = [] -- TODO
      -- inits' <- typeCheckInitDecls staticWPol constrWPol inits
      -- mDs'   <- typeCheckMemberDecls staticWPol constrWPol memberDecls
      return (inits' ++ map (MemberDecl Nothing) mDs')

typeCheckCd _ _ _ = panic (typeCheckerBase ++ ".typeCheckCd")
                  "Enum decls not yet supported"

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


typeCheckInitDecls :: ActorPolicy -> ActorPolicy -> [Decl PA] -> TcDeclM [Decl TC]
typeCheckInitDecls sLim cLim is = error "typeCheckInitDecls: not implemented"

--------------------------------------------------------------------------------
-- Bodies
--------------------------------------------------------------------------------

typeCheckMemberDecls :: ActorPolicy
                     -> ActorPolicy
                     -> [MemberDecl PA] -> TcDeclM [MemberDecl TC]
typeCheckMemberDecls sLim cLim ms = do
  st <- setupStartState
  mapM (typeCheckMemberDecl sLim cLim st) ms


typeCheckMemberDecl :: ActorPolicy -> ActorPolicy -> CodeState -> TypeCheck TcDeclM MemberDecl
typeCheckMemberDecl sLim cLim st fd@FieldDecl{} =
    typeCheckFieldDecl sLim cLim st fd
typeCheckMemberDecl _ _ st md@MethodDecl{} =
    typeCheckMethodDecl st md
typeCheckMemberDecl _ _ st cd@ConstructorDecl{} =
    typeCheckConstrDecl st cd
typeCheckMemberDecl _ _ _ md = return $ {-notAppl-} md

typeCheckFieldDecl :: ActorPolicy -> ActorPolicy -> CodeState -> TypeCheck TcDeclM MemberDecl
typeCheckFieldDecl staticLim constrLim st (FieldDecl _ ms _t vds) = error "typeCheckFieldDecl: not implemented"

typeCheckMethodDecl :: CodeState -> TypeCheck TcDeclM MemberDecl
typeCheckMethodDecl st (MethodDecl _ ms tps _rt i ps _exs mb) = error "typeCheckMethodDecl: not implemented"

typeCheckConstrDecl :: CodeState -> TypeCheck TcDeclM MemberDecl
typeCheckConstrDecl st (ConstructorDecl _ ms tps ci ps _exs cb) = error "typeCheckConstrDecl: not implemented"
