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


----------------------------------------------------------------------------------------------------------------------------------
-- ---------------------------------------------------------------------------------------------------------------------------- --
-- -- ---------------------------------------------------------------------------------------------------------------------- -- --
-- -- -- ---------------------------------------------------------------------------------------------------------------- -- -- --
-- -- -- --  INFO OM VILKET ALTERNATIV VI SKA ANVÄNDA. Very very sucky anteckning.                                     -- -- -- --
-- -- -- --  Använder typfamiljer på CodeM.Alternativ är bästa utgångspunkten.                                       -- -- -- --
-- -- -- -- Segregera env typen. Så att ni har många olika env typer, en för varje fas som kommer se väldigt olika ut. -- -- -- --
-- -- -- --                                                                                                            -- -- -- --
-- -- -- ---------------------------------------------------------------------------------------------------------------- -- -- --
-- -- ---------------------------------------------------------------------------------------------------------------------- -- --
-- ---------------------------------------------------------------------------------------------------------------------------- --
----------------------------------------------------------------------------------------------------------------------------------


codeEnvModule :: String
codeEnvModule = typeCheckerBase ++ ".Monad.CodeEnv"


-- | TODO: good docstring
data CodeEnv = CodeEnv  {
      vars      :: [Map B.ByteString VarFieldSig],
      -- ^ New list every time we enter new scope, method parameters.
      -- Make it a data type for each phase?

      -- ? Variables in scope. Each element in the list is a scope where the innermost is first.
      -- Does not contain fields -- the top level is method parameters.
      lockstate :: TcLockSet,
      -- ^ Not relevant until phase 3. This needs to be different code states.
      -- Open: global state
      -- ScopedOpen : Locally open, this ends up in this env
      -- Needed in phase 3, in AST for phase 4

      -- ? Locks that are known to be open inside the environment? /Scoped/ version, what does this mean?
      returnI   :: Maybe (Type TC, ActorPolicy),
      -- ^ ? Explain!
      -- 'Nothing' is used for void.
      -- Dem krav som finns på värdet som returneras. Varje gång vi stöter på return i koden så behöver vi kolla att vi uppfyller dem.
      -- Nothing is used to represent void. (Also Nothing when checking fields or stuff which are not methods.)
      -- Policy not needed until phase 4, because its only used to check that the returned values uppfyller policyn
      -- In phase 2 and 3 we wont need this.

      exnsE     :: Map (Type TC) (ActorPolicy, ActorPolicy),
      -- ^ ? Something about policies of exceptions
      -- Same thing here, they are not needed until we generate constraints, phase 4.

      branchPCE :: (Map Entity [(ActorPolicy, String)], [(ActorPolicy, String)]),
      -- ^ What is this? Niklas: You have two things: This is a tuple, ett ögonblick, ungen skriker, ambulansen tjuter, to be continued...
      -- Niklas is now back, ready to rumble
      -- The other component is a program counter, actor policies here are policies on those things we have branched on.
      -- Init to write modifier on method.
      -- The first component is a map from enteties to those lists. Look at exception Entities.
      -- Om jag har kastat ett exception, dels så kommer jag kolla att jag får lov att kasta execption i den branch jag är
      -- Därefter kommer allting som händer.. om någon kan se därefter en sidoeffekt kommer dem veta att vi kastade inte
      -- detta exceptionen, därför kommer det som är satt som write exception vilket då ligger i mmmmmm nånting. Kommer från den punkten
      -- bli ett constraint på (alla sidoeffekter) tills vi når en catch clause eller slutet på metoden.
      -- same here, phase 4
      parBounds :: [(B.ByteString, ActorPolicy)], -- TODO: maybe convert to `Map`?
      -- ^ Policies of method parameters
      -- Same here, actor policy are evaluated in phase 2. This will be put in the AST
      compileTime :: Bool,
      -- ^ Om vi skulle evaluera type methods fullt ut skulle dem interpreteras i fas 2. Dem behöver ha gått igenom alla faser innan vi kan köra fas
      -- 2 för andra policies. Type methods kan generera policies. (+) Ser jag inte som en type method, det är interpret modulerna som gör detta.
      -- Compiletime methods har stora begräningsngar för vad dem får innehålla. Dem kollas mot denna flaggan. När vi vet att vi kollar en type method då
      -- har vi lite extra constraints i fas 1 redan som vi behöver ta hänsyn till. När vi utvärderar en type method spelar flaggan ingen roll.
      staticContext :: Bool
    }
  deriving (Show, Data, Typeable)

data TcCodeEnv = TcCodeEnv
      { tcVars    :: [Map B.ByteString VarFieldSig]
      , tcReturnI :: Maybe (Type TC)
      , tcStaticContext :: Bool
      }
  deriving (Show, Data, Typeable)

tcSimpleEnv :: Bool -> TcCodeEnv
tcSimpleEnv statCtx =
  TcCodeEnv {
    tcVars = [emptyVarMap],
    tcReturnI = Nothing,
    tcStaticContext = statCtx
  }

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

