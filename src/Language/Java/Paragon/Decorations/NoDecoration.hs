{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- Hide errors due to our template Haskell pattern synonyms not having type
-- signatures.
{-# OPTIONS_GHC -Wno-missing-pattern-synonym-signatures #-} 
module Language.Java.Paragon.Decorations.NoDecoration where

import Language.Java.Paragon.Decorations.DecorationTypes
import Language.Java.Paragon.Syntax

import Data.Data (Data)

-- | Undecorated. AST type index for when no extension fields are used.
data UD deriving Data

-- Derive type instances on the form
-- > type instance XCompilationUnit UD = NoFieldExt
-- for all extension fields.
$(makeTypeInsts ''UD ''NoFieldExt allFamilies)

-- Make pattern synonyms on the form
-- > pattern UdCompilationUnit mpd id td = CompilationUnit () mpd id td
$(makePatternSyns "Ud" allDataConstructors [p| () |])

