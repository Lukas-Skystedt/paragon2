module Language.Java.Paragon.NewTypeCheck where
import Language.Java.Paragon.SyntaxTTG
import Language.Java.Paragon.TypesTTG
import Language.Java.Paragon.Monad.Base
import Language.Java.Paragon.Decorations.PaDecoration
import Language.Java.Paragon.Decorations.TcDecoration
-- import Language.Java.Paragon.TypeCheck.Monad.TcCodeM

type TypeCheck m ast = ast Pa -> m (ast Tc)

typeCheck ::
  -- PiPath -- ^ Paths to pi files
  -- ->
          String -- ^ Base name of the file
          -> CompilationUnit Pa
          -> BaseM (CompilationUnit Tc)
-- typeCheck baseName (CompilationUnit _pos pkg imps [_td]) = do
--   let mPkgPrefix = fmap (\(PackageDecl _ n) -> n) pkg
--   typedTd <- typeCheckTd baseName mPkgPrefix fullTd
--   return $ CompilationUnit
--            Nothing
--            (fmap notAppl pkg)
--            (map notAppl imps)
--            [typedTd]
typeCheck _ _ = error "typeCheck not implemented"
-- typeCheck _ _ =
--   fail $ "\n\n" ++ "Encountered multiple type declarations in the same file"


-- TODO: This is a placeholder type for TcDeclM
type TcDeclM = BaseM

typeCheckTd :: a
typeCheckTd = error "typeCheckId not implemented"
-- typeCheckTd :: String -> Maybe (Name Pa) -> TypeCheck TcDeclM TypeDecl
-- typeCheckTd baseName mpkg (ClassTypeDecl     _pos cdecl)
--     = ClassTypeDecl Nothing <$> typeCheckCd baseName mpkg cdecl
-- typeCheckTd baseName mpkg (InterfaceTypeDecl _pos idecl)
--     = InterfaceTypeDecl Nothing <$> typeCheckId baseName mpkg idecl

typeCheckId :: a
typeCheckId = error "typeCheckId not implemented"

typeCheckCd :: a
typeCheckCd = error "typeCheckCd not implemented"
