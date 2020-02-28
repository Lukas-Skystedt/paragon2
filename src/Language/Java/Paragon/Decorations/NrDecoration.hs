{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternSynonyms #-}

module Language.Java.Paragon.Decorations.NrDecoration where
import Language.Java.Paragon.Decorations.DecorationTypes
import Language.Java.Paragon.SourcePos
import Language.Java.Paragon.TypesTTG
import Language.Java.Paragon.SyntaxTTG
import Data.Void

data NR

type instance XCompilationUnit     NR = NoFieldExt
type instance XPackageDecl         NR = NoFieldExt
type instance XImportDecl          NR = NoFieldExt
type instance XTypeDecl            NR = NoFieldExt
type instance XClassDecl           NR = NoFieldExt
type instance XClassBody           NR = NoFieldExt
type instance XEnumBody            NR = NoFieldExt
type instance XEnumConstant        NR = NoFieldExt
type instance XInterfaceDecl       NR = NoFieldExt
type instance XInterfaceBody       NR = NoFieldExt
type instance XDecl                NR = NoFieldExt
type instance XMemberDecl          NR = NoFieldExt
type instance XVarDecl             NR = NoFieldExt
type instance XVarDeclId           NR = NoFieldExt
-- type instance XInitExp             NR = NoFieldExt
type instance XFormalParam         NR = NoFieldExt
type instance XMethodBody          NR = NoFieldExt
type instance XConstructorBody     NR = NoFieldExt
type instance XExplConstrInv       NR = NoFieldExt
type instance XMod                 NR = NoFieldExt
type instance XBlock               NR = NoFieldExt
type instance XBlockStm            NR = NoFieldExt
type instance XStm                 NR = NoFieldExt
type instance XCatch               NR = NoFieldExt
type instance XSwitchBlock         NR = NoFieldExt
type instance XSwitchLabel         NR = NoFieldExt
type instance XForInit             NR = NoFieldExt
type instance XExceptionSpec       NR = NoFieldExt
type instance XExp                 NR = NoFieldExt
type instance XLiteral             NR = NoFieldExt
type instance XOp                  NR = NoFieldExt
type instance XAssignOp            NR = NoFieldExt
type instance XLhs                 NR = NoFieldExt
type instance XArrayIndex          NR = NoFieldExt
type instance XFieldAccess         NR = NoFieldExt
type instance XMethodInvocation    NR = NoFieldExt
type instance XArrayInit           NR = NoFieldExt
type instance XReturnType          NR = NoFieldExt
type instance XType                NR = NoFieldExt
type instance XRefType             NR = NoFieldExt
type instance XClassType           NR = NoFieldExt
type instance XTypeArgument        NR = NoFieldExt
type instance XNonWildTypeArgument NR = NoFieldExt
type instance XWildcardBound       NR = NoFieldExt
type instance XPrimType            NR = NoFieldExt
type instance XTypeParam           NR = NoFieldExt
type instance XPolicyExp           NR = NoFieldExt
type instance XLockProperties      NR = NoFieldExt
type instance XClause              NR = NoFieldExt
type instance XClauseVarDecl       NR = NoFieldExt
type instance XClauseHead          NR = NoFieldExt
type instance XLClause             NR = NoFieldExt
type instance XActor               NR = NoFieldExt
type instance XActorName           NR = NoFieldExt
type instance XAtom                NR = NoFieldExt
type instance XLock                NR = NoFieldExt
type instance XIdent               NR = NoFieldExt
