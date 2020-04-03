{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
module Language.Java.Paragon.Decorations.PaDecoration where

import Language.Java.Paragon.Decorations.DecorationTypes
import Language.Java.Paragon.SourcePos
import Language.Java.Paragon.Syntax
import Language.Java.Paragon.SyntaxInstances


import Data.Data (Data,Typeable)
import Data.List ((\\))

-- | Parser. AST type index for the result of the name resolution phase.
data PA deriving Data


type instance XTypeArgumentExp PA = PaDecTypeArg
type instance XRefTypeArrayType PA = (SourcePos, [Maybe (Policy PA)])

pattern PaArrayType sp typ pol = ArrayType (sp,pol) typ

-- | Extension constructors for 'TypeArg PA'.
--
-- This type should not be used outside of this module. Instead, the pattern
-- synonyms:
-- should be used instead.
data PaDecTypeArg
  = Wildcard  SourcePos (Maybe (WildcardBound PA))
  | ActualArg SourcePos (NonWildTypeArgument PA)
  -- deriving (Eq, Show, Data, Typeable)

deriving instance Eq PaDecTypeArg
deriving instance Show PaDecTypeArg
deriving instance Ord PaDecTypeArg
deriving instance Data PaDecTypeArg
deriving instance Typeable PaDecTypeArg

pattern PaWildcard sp mwb = TypeArgumentExp (Wildcard sp mwb)
pattern PaActualArg sp nwta = TypeArgumentExp (ActualArg sp nwta)



-- Derive type instances on the form
-- > type instance XCompilationUnit PA = SourcePos
-- for all extension fields.
$(makeTypeInsts ''PA ''SourcePos
  (allFamilies \\ [''XTypeArgumentExp, ''XRefTypeArrayType]))
