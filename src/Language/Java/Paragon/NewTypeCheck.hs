-- | Top module for the type checking phase. It follows name resolution and is
-- followed by policy type evaluation.
module Language.Java.Paragon.NewTypeCheck where

import Language.Java.Paragon.SyntaxTTG
import Language.Java.Paragon.Decorations.PaDecoration
import Language.Java.Paragon.Decorations.TcDecoration
import Language.Java.Paragon.TypeCheck.Monad.TcCodeM
-- import Language.Java.Paragon.TypeCheck.Monad.TcCodeM

-- | Top level function in the type checking phase.
typeCheck :: CompilationUnit PA -> a -- TcCodeM (CompilationUnit Tc)
typeCheck = error "typeCheck placeholder evaluated"
