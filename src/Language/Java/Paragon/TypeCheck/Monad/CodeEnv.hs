{-# LANGUAGE CPP, DeriveDataTypeable, RelaxedPolyRec #-}
module Language.Java.Paragon.TypeCheck.Monad.CodeEnv where

import Language.Java.Paragon.Syntax
import Language.Java.Paragon.Interaction
import Language.Java.Paragon.SourcePos

import Language.Java.Paragon.PolicyLang
import Language.Java.Paragon.TypeCheck.Types

import Language.Java.Paragon.Decorations.PaDecoration
import Language.Java.Paragon.TypeCheck.TypeMap

import qualified Data.Map as Map
import qualified Data.ByteString.Char8 as B
import Data.Maybe(fromMaybe)

import Data.Data

codeEnvModule :: String
codeEnvModule = typeCheckerBase ++ ".Monad.CodeEnv"


-- | TODO: good docstring
data CodeEnv = CodeEnv  {
      vars      :: [Map B.ByteString VarFieldSig],
      -- ? Variables in scope. Each element in the list is a scope where the innermost is first.
      -- Does not contain fields -- the top level is method parameters.
      lockstate :: TcLockSet,
      -- ? Locks that are known to be open inside the environment? /Scoped/ version, what does this mean?
      returnI   :: Maybe (Type TC, ActorPolicy),
      -- ^ ? Explain! 'Nothing' is used for void.
      exnsE     :: Map (Type TC) (ActorPolicy, ActorPolicy),
      -- ^ ? Something about policies of exceptions
      branchPCE :: (Map Entity [(ActorPolicy, String)], [(ActorPolicy, String)]),
      -- ^ What is this?
      parBounds :: [(B.ByteString, ActorPolicy)], -- TODO: maybe convert to `Map`?
      -- ^ Policies of method parameters
      compileTime :: Bool,
      staticContext :: Bool
    }
  deriving (Show, Data, Typeable)
 
-- Env to use when typechecking expressions not inside method
-- bodies, e.g. in field initializers and policy modifiers
simpleEnv :: ActorPolicy -> Bool -> String -> Bool -> CodeEnv
simpleEnv brPol compT str statCtx =
    CodeEnv {
      vars = [emptyVarMap],
      lockstate = LockSet [],
      returnI = Nothing,
      exnsE = Map.empty,
      branchPCE = (Map.empty, [(brPol,str)]),
      parBounds = [],
      compileTime = compT,
      staticContext = statCtx
    }

-- | ? Documentation
data Entity = VarEntity (Name PA)
            | ThisFieldEntity B.ByteString
            | ExnEntity (Type TC)
            | LockEntity (Name PA)
            | BreakE | ContinueE | ReturnE | NullE
  deriving (Show, Eq, Ord, Data, Typeable)

varE, lockE :: Name PA -> Entity
varE = VarEntity
lockE = LockEntity

exnE :: Type TC -> Entity
exnE = ExnEntity

thisFE :: B.ByteString -> Entity
thisFE = ThisFieldEntity

breakE, continueE, returnE, nullE :: Entity
breakE = BreakE
continueE = ContinueE
returnE = ReturnE
nullE = NullE

emptyVarMap :: Map B.ByteString VarFieldSig
emptyVarMap = Map.empty

--------------------------------------
--    Working with the branchPC     --
--------------------------------------

branchPC :: Maybe Entity -> CodeEnv -> [(ActorPolicy, String)]
branchPC men CodeEnv{ branchPCE = (bm, def) } =
    flip (maybe def) men $ \en ->
        fromMaybe def (Map.lookup en bm)

joinBranchPC :: ActorPolicy -> String -> CodeEnv -> CodeEnv
joinBranchPC p str env = let (bm, def) = branchPCE env
                         in env { branchPCE = (Map.map ((p, str):) bm, (p,str):def) }
