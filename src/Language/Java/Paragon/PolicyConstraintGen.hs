module Language.Java.Paragon.PolicyConstraintGen where
import Language.Java.Paragon.SyntaxTTG
import Language.Java.Paragon.Decorations.LseDecoration
import Language.Java.Paragon.Decorations.PcgDecoration
import Language.Java.Paragon.TypeCheck.Monad.TcCodeM

genPolicyConstraints :: CompilationUnit Lse -> TcCodeM (CompilationUnit Pcg)
genPolicyConstraints = error "genPolicyConstraints placeholder evaluated"
