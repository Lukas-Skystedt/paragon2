{-# LANGUAGE PatternGuards, BangPatterns #-}
-- | Module for typechecking expressions.
module Language.Java.Paragon.TypeCheck.TcExp where

import Language.Java.Paragon.Syntax
import Language.Java.Paragon.Pretty
import Language.Java.Paragon.Interaction
import Language.Java.Paragon.Error
import Language.Java.Paragon.SourcePos

import qualified Language.Java.Paragon.PolicyLang as PL

import Language.Java.Paragon.Decorations.PaDecoration
import Language.Java.Paragon.TypeCheck.Monad.TcCodeM
import Language.Java.Paragon.TypeCheck.Monad
import Language.Java.Paragon.TypeCheck.Types
import Language.Java.Paragon.TypeCheck.TypeMap
import Language.Java.Paragon.TypeCheck.NullAnalysis

import Language.Java.Paragon.TypeCheck.NotAppl

import Data.List ((\\))
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Map as Map
import Control.Applicative ( (<$>) )
import Control.Monad (when, foldM, forM_, zipWithM)

import qualified Data.ByteString.Char8 as B (pack)

-----------------------------------
--    Checking expressions       --
-----------------------------------

-- TODO: This documentation is outdated!!!
-- | Typechecks a term that is parsed as some expression and returns a triple
-- consisting of the (state) type of that expression, the policy on the
-- expression, and a typechecked expression.
-- Encapsulated in the TcCodeM monad gives access to the code environment,
-- state, allows it to fail, add error messages and policy contraints.
tcExp :: Exp PA -> TcCodeM (TcStateType, Exp TC)

-- Rule LIT
-- Literals simply look up their state type. Their policy defaults to bottom
-- (we might eventually want to infer the appropriate policy instead).
tcExp (Lit loc l) = do
  sty <- getStateType Nothing Nothing $ litType l
  return (sty, Lit (toT sty) (notAppl l))

-----------------------------------
--    Types of literal values    --
-----------------------------------

litType :: Literal PA -> Type TC
litType (Int     _ _) = intT
litType (Word    _ _) = longT
litType (Float   _ _) = floatT
litType (Double  _ _) = doubleT
litType (Boolean _ _) = booleanT
litType (Char    _ _) = charT
litType (String  _ _) = clsTypeToType stringT
litType (Null    _  ) = nullT
