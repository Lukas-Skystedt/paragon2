{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
-- | This module contains template Haskell definitions used by
-- 'Language.Java.Paragon.Syntax'. They have their own module since template
-- Haskell disallows them from being defined and used in the same module.
module Language.Java.Paragon.SyntaxInstances where

import Language.Haskell.TH
import Control.Monad (join)

-- | Generate a standalone instance derivations for the provided classes, types
-- and constraints using 'deriveInstance'. The same constraint is used in all
-- instances.
deriveInstances :: Name -> [Name] -> [Name] -> DecsQ
deriveInstances constraint clazzes types
  = fmap join $ sequence $
    [deriveInstance constraint clazz typ | clazz <- clazzes
                                         , typ   <- types]

-- | Generate a standalone instance derivation for the provided class and type
-- and constraint(s). It is written to work with the type families used in the
-- syntax tree.
--
-- The resulting instance derivation has the form.
--
-- > deriving instance constraint clazz x => clazz (typ x)
deriveInstance :: Name -> Name -> Name -> DecsQ
deriveInstance constraint clazz typ =
  [d| deriving instance $con $c x => $c ($t x) |]
  where con = conT constraint
        c   = conT clazz
        t   = conT typ
