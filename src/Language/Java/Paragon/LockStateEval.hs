module Language.Java.Paragon.LockStateEval where
import Language.Java.Paragon.SyntaxTTG
import Language.Java.Paragon.Decorations.PteDecoration
import Language.Java.Paragon.Decorations.LseDecoration
import Language.Java.Paragon.TypeCheck.Monad.TcCodeM

evalLockState :: CompilationUnit Pte -> TcCodeM (CompilationUnit Lse)
evalLockState = error "evalPolicyTypes placeholder evaluated"
