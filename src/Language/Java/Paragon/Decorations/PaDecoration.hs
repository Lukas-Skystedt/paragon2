{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Java.Paragon.Decorations.PaDecoration where
import Language.Java.Paragon.Decorations.DecorationTypes
import Language.Java.Paragon.SourcePos
import Language.Java.Paragon.TypesTTG
import Language.Java.Paragon.SyntaxTTG
import Data.Void
import Data.Data
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

data Pa deriving (Data, Eq, Show)

-- This is a hack.
type instance XOp () = ()

-- Derive type instances on the form
-- > type instance XCompilationUnit     Pa = SourcePos
-- for all extension fields.
$(makeTypeInsts ''Pa ''SourcePos allFamilies)
