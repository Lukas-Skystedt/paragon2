{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
module Language.Java.Paragon.Decorations.PteDecoration where

import Language.Java.Paragon.Decorations.DecorationTypes
import Language.Java.Paragon.SyntaxTTG

-- | Policy Type Evaluation. AST type index for the result of the policy type
-- evaluation phase.
data PTE

-- Derive type instances on the form
-- > type instance XCompilationUnit PTE = NoFieldExt
-- for all extension fields.
$(makeTypeInsts ''PTE ''NoFieldExt allFamilies)

