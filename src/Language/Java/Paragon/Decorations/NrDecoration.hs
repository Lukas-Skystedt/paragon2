{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
module Language.Java.Paragon.Decorations.NrDecoration where

import Language.Java.Paragon.Decorations.DecorationTypes
import Language.Java.Paragon.SyntaxTTG

-- | Name Resolution. AST type index for the result of the name resolver phase.
data NR

-- Derive type instances on the form
-- > type instance XCompilationUnit NR = NoFieldExt
-- for all extension fields.
$(makeTypeInsts ''NR ''NoFieldExt allFamilies)
