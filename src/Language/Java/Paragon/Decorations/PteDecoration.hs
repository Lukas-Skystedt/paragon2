{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Language.Java.Paragon.Decorations.PteDecoration where

import Language.Java.Paragon.Decorations.DecorationTypes
import Language.Java.Paragon.Syntax
import Language.Java.Paragon.PolicyLang
import Language.Java.Paragon.SourcePos (SourcePos)

import Data.List ((\\))
import Data.Data (Data)

-- | Policy Type Evaluation. AST type index for the result of the policy type
-- evaluation phase.
data PTE deriving Data

type PolicyDec = ActorPolicyBounds

-- Derive type instances on the form
-- > type instance XCompilationUnit PTE = NoFieldExt
$(makeTypeInsts ''PTE ''NoFieldExt (allFamilies \\ [''XName, ''XIdent]))

type instance XIdent PTE = SourcePos
type instance XName  PTE = SourcePos

$(makeTypeInsts ''PTE ''PolicyDec [])

$(makePatternSyns "Pte"
  (allDataConstructors \\ [])
  [p| () |])