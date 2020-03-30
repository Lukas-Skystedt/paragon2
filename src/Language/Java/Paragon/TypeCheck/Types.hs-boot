{-# LANGUAGE CPP #-}
module Language.Java.Paragon.TypeCheck.Types where

import Language.Java.Paragon.Pretty
import Language.Java.Paragon.Syntax

import Control.Applicative

#ifdef BASE4
import Data.Data
#else
import Data.Generics (Data(..),Typeable(..))
#endif

import qualified Control.Monad.Fail as Fail

data TC
instance Data TC
-- data TcRefType

-- instance Eq TcRefType
-- instance Ord TcRefType
-- instance Show TcRefType
-- instance Data TcRefType
-- -- instance Typeable TcRefType
-- instance Pretty TcRefType

class (Functor m, Applicative m, Monad m, Fail.MonadFail m) => HasSubTyping m where
  subTypeOf :: RefType TC -> RefType TC -> m Bool
