{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}

-- Hide errors due to our template Haskell pattern synonyms not having type
-- signatures.
{-# OPTIONS_GHC -Wno-missing-pattern-synonym-signatures #-} 
module Language.Java.Paragon.Decorations.TcDecoration where

import Language.Java.Paragon.Decorations.DecorationTypes
import Language.Java.Paragon.TypeCheck.Types
import Language.Java.Paragon.SyntaxTTG
-- import Language.Java.Paragon.TypeCheck.Types

import Data.List ((\\))

-- | Type checking. AST type index for the result of the type checking phase.
data TC

type instance XType TC = NoExtField
type instance XPrimType TC = NoExtField
type instance XRefType TC = NoExtField
type instance XClassType TC = NoExtField
type instance XName TC = UD
type instance XTypeArgument = TcTypeArg

pattern TcActualType rt = TypeArgumentExp (TcDecActualType rt)
pattern TcActualPolicy ap = TypeArgumentExp (TcActualPolicy ap)
pattern TcActualActor taid = TypeArgumentExp (TcActualActor taid)
pattern TcActualLockState ls =TypeArgumentExp (TcActualLockState ls)

type instance XRefTypeExp TC = NoExtField

pattern TcNullT = RefTypeExp ()

-- Derive type instances on the form
-- > type instance XCompilationUnit PTE = NoFieldExt
-- or
-- > type instance XExp PTE = TcPlaceHolder
-- for all extension fields.
--
-- The former is for types that don't have Paragon types, the latter for types
-- that do.

$(makeTypeInsts ''TC ''NoFieldExt
  [ ''XCompilationUnit, ''XPackageDecl, ''XImportDecl, ''XTypeDecl, ''XClassDecl
  , ''XClassBody, ''XEnumBody, ''XEnumConstant, ''XInterfaceDecl, ''XInterfaceBody
  , ''XDecl
  ])

-- Make pattern synonyms on the form
-- > pattern TcCompilationUnit typ mpd id td = CompilationUnit typ mpd id td
$(makePatternSyns "Tc" allDataConstructors [p| () |])

$(makeTypeInsts ''TC ''T
  [ ''XMemberDecl, ''XVarDecl, ''XVarDeclId, ''XFormalParam, ''XMethodBody
  , ''XConstructorBody, ''XExplConstrInv, ''XMod, ''XBlock, ''XBlockStm
  , ''XStm, ''XCatch, ''XSwitchBlock, ''XSwitchLabel, ''XForInit
  , ''XExceptionSpec, ''XExp, ''XLiteral, ''XOp, ''XAssignOp, ''XLhs
  , ''XArrayIndex, ''XFieldAccess, ''XMethodInvocation, ''XArrayInit
  , ''XReturnType, ''XType, ''XRefType, ''XClassType, ''XTypeArgument
  , ''XNonWildTypeArgument, ''XWildcardBound, ''XPrimType, ''XTypeParam
  , ''XPolicyExp, ''XLockProperties, ''XClause, ''XClauseVarDecl, ''XClauseHead
  , ''XLClause, ''XActor, ''XActorName, ''XAtom, ''XLock, ''XIdent
  ]
  )
