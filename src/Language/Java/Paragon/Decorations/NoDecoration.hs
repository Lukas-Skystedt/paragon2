{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Java.Paragon.Decorations.NoDecoration where

import Language.Java.Paragon.Decorations.DecorationTypes
import Language.Java.Paragon.TypesTTG
import Language.Java.Paragon.SyntaxTTG

data UD

-- Derive type instances on the form
-- > type instance XCompilationUnit UD = NoFieldExt
-- for all extension fields.
$(makeTypeInsts ''UD ''NoFieldExt allFamilies)

-- Make pattern synonyms on the form
-- > pattern UdCompilationUnit mpd id td = CompilationUnit () mpd id td
$(makePatternSyns "Ud" allDataConstructors [p| () |])

