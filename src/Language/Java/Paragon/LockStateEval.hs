-- | Top module for the lock state evaluation phase. It follows policy type
-- evaluation and is followed by policy constraint generation.
module Language.Java.Paragon.LockStateEval where

import Language.Java.Paragon.Syntax
import Language.Java.Paragon.Decorations.PteDecoration
import Language.Java.Paragon.Decorations.LseDecoration
-- import Language.Java.Paragon.TypeCheck.Monad.CodeM

-- | Top level function in the lock state evaluation phase.
evalLockState :: CompilationUnit PTE -> a --CodeM (CompilationUnit Lse)
evalLockState = error "evalPolicyTypes placeholder evaluated"
