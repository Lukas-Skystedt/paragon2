{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
module Language.Java.Paragon.Decorations.PcgDecoration where

import Language.Java.Paragon.Decorations.DecorationTypes
import Language.Java.Paragon.SyntaxTTG

-- | Policy Constraint Generation. AST type index for the result of the policy
-- constraint generation phase.
data PCG

-- Derive type instances on the form
-- > type instance XCompilationUnit PCG = NoFieldExt
-- for all extension fields.
$(makeTypeInsts ''PCG ''NoFieldExt allFamilies)
