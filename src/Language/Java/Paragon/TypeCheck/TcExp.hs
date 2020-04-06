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

tcExpModule :: String
tcExpModule = typeCheckerBase ++ ".TcExp"
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

tcExp (Paren _ e) = do (ty, e') <- tcExp e
                       return (ty, Paren (toT ty) e')

-- Rule THIS
-- Simple wrapping, policy defaults to bottom.
tcExp e@(This sp) = error "tcExp: case This not implemented"

-- Rule BINOP
-- Any binary operation consists of checking the two sub-expressions and
-- joining the policies of each.
tcExp (BinOp _ e1 op e2) = error "tcExp: case BinOp not implemented"

-- Rule VAR/FIELD
-- If we have a regular variable / field we simply look up its type and policy.
-- If we have a lock we additionaly check its arity. If we do now know if it is
-- a lock or not, we try assuming it is a lock first.
tcExp (ExpName _ n) = 
    case n of
      Name sp EName mPre i -> do
             (ty, _pol, _, mStatFld) <- lookupVar mPre i
             debugPrint $ "lookupVar found: " ++ show ty
             staticCtx <- getStaticContext
             check (not staticCtx || fromMaybe True mStatFld) $
               mkError (NonStaticFieldReferencedFromStatic (prettyPrint i)) sp
             return (ty, ExpName (toT ty) (notAppl n))
      Name sp LName mPre i -> do
             LSig _pL lTys _ <- lookupLock mPre i
             check (null lTys) $
               mkError (LArityMismatch (prettyPrint n) (length lTys) 0) sp
             let ty = lockT [PL.ConcreteLock $ PL.Lock n []]
             return (ty, ExpName (toT ty) (notAppl n))
      Name sp EOrLName mPre i -> do
             tryCatch (tcExp $ ExpName sp $ Name sp LName mPre i)
                (\ec ->tcExp $ ExpName sp $ Name sp EName mPre i)

      _ -> panic (tcExpModule ++ ".tcExp:ExpName")
           $ "Unexpected name: " ++ show n

-- Rule VARASS/FIELDASS
-- Fairly extensive check. We first check the left-hand side (lhs) of the
-- assignment to see if the location exists and if it is allowed to be updated
-- in the current context.
-- We then continue to check the assignment itself to see if the there are any
-- policy violations.
tcExp ex@(Assign exSp lhs _op rhs) = error "tcExp: case Assign not implemented"
  
-- Rule CALL
-- Redirected into separate function
tcExp (MethodInv _ mi) = do
  (ty, mi') <- tcMethodOrLockInv mi
  return (ty, MethodInv (toT ty) mi')

-- Rule NEW
-- Redirected into separate function
tcExp (InstanceCreation _ tas ct args Nothing) = error "cExp: case InstanceCreation not implemented"
   
-- Rule COND
-- Recursively check the branches (with increased pc) and check if types of
-- branches match.
tcExp (Cond sp c e1 e2) = error "tcExp: case Cond not implemented"


-- Rule POLICYEXP
-- Redirected into separate function
tcExp (PolicyExp _ pl) = error "tcExp: case PolicyExp not implemented"

-- Rule POST/PRE-INCREMENT/DECREMENT
-- Basically only check that operator is used on numeric type
tcExp (PostIncrement sp e) = error "tcExp: case PostIncrement not implemented"

-- Rule PREP
-- Other unary operators.
-- All just check if operator is applied on correct type, except cast.
tcExp (PrePlus sp e) = error "tcExp: case PrePlus not implemented"

tcExp (Cast sp t e)  = error "cExp: case Cast not implemented"

-- Rule FIELDACCESS
-- Redirected into separate function
tcExp (FieldAccess _ fa) = do
  (ty, fa') <- tcFieldAccess fa
  return (ty, FieldAccess (toT ty) fa')

----------
-- Arrays
----------

-- Rule ARRAYCREATE
tcExp (ArrayCreate sp bt dimEsPs dimImplPs) = error "tcExp: case ArrayCreate not implemented"

-- Rule ARRAYCREATEINIT
tcExp (ArrayCreateInit _ bt dimImplPs arrInit) = error "tcExp: case ArrayCreateInit not implemented"

-- Rule ARRAYACCESS
tcExp (ArrayAccess spA (ArrayIndex spI arrE iE)) =  error "tcExp: case ArrayAccess not implemented"


tcFieldAccess :: FieldAccess PA -> TcCodeM (TcStateType, FieldAccess TC)
tcFieldAccess (PrimaryFieldAccess _ e fi) = error "tcFieldAccess not implemented"
--   do
-- (tyE,pE,e') <- tcExp e
-- VSig tyF pFi _ _ _ _ <- instThis pE =<< lookupFieldT tyE fi
-- styF <- getStateType Nothing Nothing tyF -- TODO: this?
-- polRes <- pE `PL.lub` pFi
-- return (styF, polRes, PrimaryFieldAccess (toT styF) e' (notAppl fi))

tcFieldAccess fa = error $ "Unsupported field access: " ++ prettyPrint fa

tcMethodOrLockInv :: MethodInvocation PA -> TcCodeM (TcStateType, MethodInvocation TC)
tcMethodOrLockInv _ = error "tcMethodOrLockInv: not implemented"

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
