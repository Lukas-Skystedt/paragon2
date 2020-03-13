{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Java.Paragon.Decorations.PcsDecoration where
import Language.Java.Paragon.Decorations.DecorationTypes
import Language.Java.Paragon.SourcePos
import Language.Java.Paragon.TypesTTG
import Language.Java.Paragon.SyntaxTTG
import Data.Void

data PCS deriving Eq
type PcsPlaceHolder = ()

-- Derive type instances on the form
-- > type instance XCompilationUnit PCS = NoFieldExt
-- for all extension fields.
$(makeTypeInsts ''PCS ''NoFieldExt allFamilies)

-- Make pattern synonyms on the form
-- > pattern PcsCompilationUnit mpd id td = CompilationUnit () mpd id td
$(makePatternSyns "Pcs" allDataConstructors [p| () |])

