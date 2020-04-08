-- | Top module for the policy type evaluation phase. It follows type checking
-- and is followed by lock state evaluation.
module Language.Java.Paragon.PolicyTypeEval where

import Language.Java.Paragon.Syntax
import Language.Java.Paragon.TypeCheck.Types
import Language.Java.Paragon.Decorations.PteDecoration
-- import Language.Java.Paragon.TypeCheck.Monad.CodeM

-- | Top level function in the policy type evaluation phase.
evalPolicyTypes :: CompilationUnit TC -> a -- CodeM (CompilationUnit Pte)
evalPolicyTypes = error "evalPolicyTypes placeholder evaluated"
