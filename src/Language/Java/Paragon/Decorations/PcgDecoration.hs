{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternSynonyms #-}

module Language.Java.Paragon.Decorations.PcgDecoration where
import Language.Java.Paragon.Decorations.DecorationTypes
import Language.Java.Paragon.SourcePos
import Language.Java.Paragon.TypesTTG
import Language.Java.Paragon.SyntaxTTG
import Data.Void

data PCG
type PcgPlaceHolder = ()

type instance XCompilationUnit     PCG = PcgPlaceHolder
type instance XPackageDecl         PCG = PcgPlaceHolder
type instance XImportDecl          PCG = PcgPlaceHolder
type instance XTypeDecl            PCG = PcgPlaceHolder
type instance XClassDecl           PCG = PcgPlaceHolder
type instance XClassBody           PCG = PcgPlaceHolder
type instance XEnumBody            PCG = PcgPlaceHolder
type instance XEnumConstant        PCG = PcgPlaceHolder
type instance XInterfaceDecl       PCG = PcgPlaceHolder
type instance XInterfaceBody       PCG = PcgPlaceHolder
type instance XDecl                PCG = PcgPlaceHolder
type instance XMemberDecl          PCG = PcgPlaceHolder
type instance XVarDecl             PCG = PcgPlaceHolder
type instance XVarDeclId           PCG = PcgPlaceHolder
-- type instance XInitExp             PCG = PcgPlaceHolder
type instance XFormalParam         PCG = PcgPlaceHolder
type instance XMethodBody          PCG = PcgPlaceHolder
type instance XConstructorBody     PCG = PcgPlaceHolder
type instance XExplConstrInv       PCG = PcgPlaceHolder
type instance XMod                 PCG = PcgPlaceHolder
type instance XBlock               PCG = PcgPlaceHolder
type instance XBlockStm            PCG = PcgPlaceHolder
type instance XStm                 PCG = PcgPlaceHolder
type instance XCatch               PCG = PcgPlaceHolder
type instance XSwitchBlock         PCG = PcgPlaceHolder
type instance XSwitchLabel         PCG = PcgPlaceHolder
type instance XForInit             PCG = PcgPlaceHolder
type instance XExceptionSpec       PCG = PcgPlaceHolder
type instance XExp                 PCG = PcgPlaceHolder
type instance XLiteral             PCG = PcgPlaceHolder
type instance XOp                  PCG = PcgPlaceHolder
type instance XAssignOp            PCG = PcgPlaceHolder
type instance XLhs                 PCG = PcgPlaceHolder
type instance XArrayIndex          PCG = PcgPlaceHolder
type instance XFieldAccess         PCG = PcgPlaceHolder
type instance XMethodInvocation    PCG = PcgPlaceHolder
type instance XArrayInit           PCG = PcgPlaceHolder
type instance XReturnType          PCG = PcgPlaceHolder
type instance XType                PCG = PcgPlaceHolder
type instance XRefType             PCG = PcgPlaceHolder
type instance XClassType           PCG = PcgPlaceHolder
type instance XTypeArgument        PCG = PcgPlaceHolder
type instance XNonWildTypeArgument PCG = PcgPlaceHolder
type instance XWildcardBound       PCG = PcgPlaceHolder
type instance XPrimType            PCG = PcgPlaceHolder
type instance XTypeParam           PCG = PcgPlaceHolder
type instance XPolicyExp           PCG = PcgPlaceHolder
type instance XLockProperties      PCG = PcgPlaceHolder
type instance XClause              PCG = PcgPlaceHolder
type instance XClauseVarDecl       PCG = PcgPlaceHolder
type instance XClauseHead          PCG = PcgPlaceHolder
type instance XLClause             PCG = PcgPlaceHolder
type instance XActor               PCG = PcgPlaceHolder
type instance XActorName           PCG = PcgPlaceHolder
type instance XAtom                PCG = PcgPlaceHolder
type instance XLock                PCG = PcgPlaceHolder
type instance XIdent               PCG = PcgPlaceHolder
