{-# LANGUAGE CPP, FlexibleInstances, 
    MultiParamTypeClasses, UndecidableInstances #-}
module Language.Java.Paragon.PolicyLang 
    (
     ActorPolicy, ActorPolicyBounds, TcLock, 
     TypedActorIdSpec, LockSpec
    ) where

import Language.Java.Paragon.SourcePos
import Language.Java.Paragon.SyntaxTTG (Name)
import Language.Java.Paragon.Pretty
import Language.Java.Paragon.Decorations.PaDecoration

import Control.Applicative

#ifdef BASE4
import Data.Data
#else
import Data.Generics (Data(..),Typeable(..))
#endif

import {-# SOURCE #-} Language.Java.Paragon.PolicyLang.Actors
import {-# SOURCE #-} Language.Java.Paragon.PolicyLang.Locks (LockSpec)
import {-# SOURCE #-} Language.Java.Paragon.PolicyLang.Policy 

import Security.InfoFlow.Policy.FlowLocks


type ActorPolicy 
    = MetaPolicy 
        MetaVarRep 
        PolicyVarRep 
        (Name PA) 
        ActorSetRep

type PrgPolicy
    = VarPolicy
        PolicyVarRep
        (Name PA)
        ActorSetRep

type TcLock = LockSpec
type TcLockDelta = LockDelta (Name PA) TypedActorIdSpec
type GlobalPol = GlobalPolicy (Name PA) ActorSetRep

data ActorPolicyBounds
instance Eq ActorPolicyBounds
instance Show ActorPolicyBounds
instance Data ActorPolicyBounds
--instance Typeable ActorPolicyBounds
instance Pretty ActorPolicyBounds

{-
instance HasSubTyping m =>
    PartialOrder m ActorPolicyBounds

instance HasSubTyping m =>
    JoinSemiLattice m ActorPolicyBounds

instance HasSubTyping m =>
    Lattice m ActorPolicyBounds
-}

