{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
module Language.Java.Paragon.Decorations.TcDecoration where

import Language.Java.Paragon.Decorations.DecorationTypes
import Language.Java.Paragon.SyntaxTTG


-- | Type checking. AST type index for the result of the type checking phase.
data TC

-- | Placeholder type that will be replaced by the actual type for Paragon types
-- (AST decoration).
type TcPlaceHolder = ()

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

$(makeTypeInsts ''TC ''TcPlaceHolder
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
