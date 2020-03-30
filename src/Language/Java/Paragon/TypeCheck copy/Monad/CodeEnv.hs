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

data CodeEnv = CodeEnv  {
      vars      :: [Map B.ByteString VarFieldSig],
      lockstate :: TcLockSet,
      returnI   :: Maybe (Type TC, ActorPolicy),
      exnsE     :: Map (Type TC) (ActorPolicy, ActorPolicy),
      branchPCE :: (Map Entity [(ActorPolicy, String)], [(ActorPolicy, String)]),
      parBounds :: [(B.ByteString, ActorPolicy)], -- TODO: maybe convert to `Map`?
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
