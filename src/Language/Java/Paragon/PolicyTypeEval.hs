module Language.Java.Paragon.PolicyTypeEval where
import Language.Java.Paragon.SyntaxTTG
import Language.Java.Paragon.Decorations
import Language.Java.Paragon.TypeCheck.Monad.TcCodeM

evalPolicyTypes :: CompilationUnit Tc -> TcCodeM (CompilationUnit Pte)
evalPolicyTypes = error "evalPolicyTypes placeholder evaluated"
