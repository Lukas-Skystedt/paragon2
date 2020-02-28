{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternSynonyms #-}

module Language.Java.Paragon.Decorations.PaDecoration where
import Language.Java.Paragon.Decorations.DecorationTypes
import Language.Java.Paragon.SourcePos
import Language.Java.Paragon.TypesTTG
import Language.Java.Paragon.SyntaxTTG
import Data.Void
import Data.Data

data Pa deriving (Data, Eq, Show)

-- This is a hack.
type instance XOp () = ()

type instance XCompilationUnit     Pa = SourcePos
type instance XPackageDecl         Pa = SourcePos
type instance XImportDecl          Pa = SourcePos
type instance XTypeDecl            Pa = SourcePos
type instance XClassDecl           Pa = SourcePos
type instance XClassBody           Pa = SourcePos
type instance XEnumBody            Pa = SourcePos
type instance XEnumConstant        Pa = SourcePos
type instance XInterfaceDecl       Pa = SourcePos
type instance XInterfaceBody       Pa = SourcePos
type instance XDecl                Pa = SourcePos
type instance XMemberDecl          Pa = SourcePos
type instance XVarDecl             Pa = SourcePos
type instance XVarDeclId           Pa = SourcePos
type instance XVarInit             Pa = SourcePos
type instance XFormalParam         Pa = SourcePos
type instance XMethodBody          Pa = SourcePos
type instance XConstructorBody     Pa = SourcePos
type instance XExplConstrInv       Pa = SourcePos
type instance XMod                 Pa = SourcePos
type instance XBlock               Pa = SourcePos
type instance XBlockStm            Pa = SourcePos
type instance XStm                 Pa = SourcePos
type instance XCatch               Pa = SourcePos
type instance XSwitchBlock         Pa = SourcePos
type instance XSwitchLabel         Pa = SourcePos
type instance XForInit             Pa = SourcePos
type instance XExceptionSpec       Pa = SourcePos
type instance XExp                 Pa = SourcePos
type instance XLiteral             Pa = SourcePos
type instance XOp                  Pa = SourcePos
type instance XAssignOp            Pa = SourcePos
type instance XLhs                 Pa = SourcePos
type instance XArrayIndex          Pa = SourcePos
type instance XFieldAccess         Pa = SourcePos
type instance XMethodInvocation    Pa = SourcePos
type instance XArrayInit           Pa = SourcePos
type instance XReturnType          Pa = SourcePos
type instance XType                Pa = SourcePos
type instance XRefType             Pa = SourcePos
type instance XClassType           Pa = SourcePos
type instance XTypeArgument        Pa = SourcePos
type instance XNonWildTypeArgument Pa = SourcePos
type instance XWildcardBound       Pa = SourcePos
type instance XPrimType            Pa = SourcePos
type instance XTypeParam           Pa = SourcePos
type instance XPolicyExp           Pa = SourcePos
type instance XLockProperties      Pa = SourcePos
type instance XClause              Pa = SourcePos
type instance XClauseVarDecl       Pa = SourcePos
type instance XClauseHead          Pa = SourcePos
type instance XLClause             Pa = SourcePos
type instance XActor               Pa = SourcePos
type instance XActorName           Pa = SourcePos
type instance XAtom                Pa = SourcePos
type instance XLock                Pa = SourcePos
type instance XIdent               Pa = SourcePos
type instance XName                Pa = SourcePos
