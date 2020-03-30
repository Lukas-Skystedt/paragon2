{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
module Language.Java.Paragon.Decorations.LseDecoration where

import Language.Java.Paragon.Decorations.DecorationTypes
import Language.Java.Paragon.Syntax

-- | Lock State Evaluation. AST type index for the result of the lock state
-- evaluation phase.
data LSE

-- Derive type instances on the form
-- > type instance XCompilationUnit LSE = NoFieldExt
-- for all extension fields.
$(makeTypeInsts ''LSE ''NoFieldExt allFamilies)
