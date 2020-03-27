-- | Top module for the type checking phase. It follows name resolution and is
-- followed by policy type evaluation.
module Language.Java.Paragon.NewTypeCheck where

import Language.Java.Paragon.TypeCheck.Types
import Language.Java.Paragon.SyntaxTTG
import Language.Java.Paragon.Decorations.PaDecoration
import Language.Java.Paragon.TypeCheck.Monad.TcCodeM
-- import Language.Java.Paragon.TypeCheck.Monad.TcCodeM


-- | Base name of a file being type checked.
-- | TODO: check what format the string is in.
type BaseName = String


-- type TypeCheck m ast = ast PA -> m (ast TC)

-- | Top level function in the type checking phase.
typeCheck :: PiPath   -- ^ Paths to pi files
          -> BaseName -- ^ Base name of the file
          -> CompilationUnit PA
          -> BaseM (CompilationUnit TC)
typeCheck = error "typeCheck placeholder evaluated"



typeCheckTd :: BaseName -> Maybe (Name PA) -> TypeCheck TcDeclM TypeDecl
typeCheckTd baseName mpkg (ClassTypeDecl     _ cdecl)
    = TcClassTypeDecl <$> typeCheckCd baseName mpkg cdecl
typeCheckTd baseName mpkg (InterfaceTypeDecl _ idecl)
  = error "typeCheckTd: not implemented"

typeCheckCd = error "typeCheckCd: not implemented"
