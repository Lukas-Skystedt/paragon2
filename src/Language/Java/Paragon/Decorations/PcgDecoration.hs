{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Java.Paragon.Decorations.PcgDecoration where
import Language.Java.Paragon.Decorations.DecorationTypes
import Language.Java.Paragon.SourcePos
import Language.Java.Paragon.TypesTTG
import Language.Java.Paragon.SyntaxTTG
import Data.Void

data PCG

-- Derive type instances on the form
-- > type instance XCompilationUnit PCG = NoFieldExt
-- for all extension fields.
$(makeTypeInsts ''PCG ''NoFieldExt allFamilies)
