-- | Top module for the policy constraint generation phase. It follows lock
-- state evaluation and is followed by policy constraint solving.
module Language.Java.Paragon.PolicyConstraintGen where

import Language.Java.Paragon.Syntax
import Language.Java.Paragon.Decorations.LseDecoration
import Language.Java.Paragon.Decorations.PcgDecoration
-- import Language.Java.Paragon.TypeCheck.Monad.CodeM

-- | Top level function in the policy constraint generation phase.
genPolicyConstraints :: CompilationUnit LSE -> a -- CodeM (CompilationUnit Pcg)
genPolicyConstraints = error "genPolicyConstraints placeholder evaluated"
