module Language.Java.Paragon.NewTypeCheck where
import Language.Java.Paragon.SyntaxTTG
import Language.Java.Paragon.Decorations
import Language.Java.Paragon.TypeCheck.Monad.TcCodeM

typeCheck :: CompilationUnit PA -> TcCodeM (CompilationUnit Tc)
typeCheck = error "typeCheck placeholder evaluated"
