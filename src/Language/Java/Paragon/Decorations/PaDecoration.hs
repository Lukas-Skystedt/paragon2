{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
module Language.Java.Paragon.Decorations.PaDecoration where

import Language.Java.Paragon.Decorations.DecorationTypes
import Language.Java.Paragon.SourcePos
import Language.Java.Paragon.SyntaxTTG

import Data.Data (Data)

-- | Parser. AST type index for the result of the name resolution phase.
data PA deriving Data

-- Derive type instances on the form
-- > type instance XCompilationUnit PA = SourcePos
-- for all extension fields.
$(makeTypeInsts ''PA ''SourcePos allFamilies)
