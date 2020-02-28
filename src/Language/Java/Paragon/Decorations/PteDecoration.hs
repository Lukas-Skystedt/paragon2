{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternSynonyms #-}

module Language.Java.Paragon.Decorations.PteDecoration where
import Language.Java.Paragon.Decorations.DecorationTypes
import Language.Java.Paragon.SourcePos
import Language.Java.Paragon.TypesTTG
import Language.Java.Paragon.SyntaxTTG
import Data.Void

data PTE
type PtePlaceHolder = ()

type instance XCompilationUnit     PTE = PtePlaceHolder
type instance XPackageDecl         PTE = PtePlaceHolder
type instance XImportDecl          PTE = PtePlaceHolder
type instance XTypeDecl            PTE = PtePlaceHolder
type instance XClassDecl           PTE = PtePlaceHolder
type instance XClassBody           PTE = PtePlaceHolder
type instance XEnumBody            PTE = PtePlaceHolder
type instance XEnumConstant        PTE = PtePlaceHolder
type instance XInterfaceDecl       PTE = PtePlaceHolder
type instance XInterfaceBody       PTE = PtePlaceHolder
type instance XDecl                PTE = PtePlaceHolder
type instance XMemberDecl          PTE = PtePlaceHolder
type instance XVarDecl             PTE = PtePlaceHolder
type instance XVarDeclId           PTE = PtePlaceHolder
-- type instance XInitExp             PTE = PtePlaceHolder
type instance XFormalParam         PTE = PtePlaceHolder
type instance XMethodBody          PTE = PtePlaceHolder
type instance XConstructorBody     PTE = PtePlaceHolder
type instance XExplConstrInv       PTE = PtePlaceHolder
type instance XMod                 PTE = PtePlaceHolder
type instance XBlock               PTE = PtePlaceHolder
type instance XBlockStm            PTE = PtePlaceHolder
type instance XStm                 PTE = PtePlaceHolder
type instance XCatch               PTE = PtePlaceHolder
type instance XSwitchBlock         PTE = PtePlaceHolder
type instance XSwitchLabel         PTE = PtePlaceHolder
type instance XForInit             PTE = PtePlaceHolder
type instance XExceptionSpec       PTE = PtePlaceHolder
type instance XExp                 PTE = PtePlaceHolder
type instance XLiteral             PTE = PtePlaceHolder
type instance XOp                  PTE = PtePlaceHolder
type instance XAssignOp            PTE = PtePlaceHolder
type instance XLhs                 PTE = PtePlaceHolder
type instance XArrayIndex          PTE = PtePlaceHolder
type instance XFieldAccess         PTE = PtePlaceHolder
type instance XMethodInvocation    PTE = PtePlaceHolder
type instance XArrayInit           PTE = PtePlaceHolder
type instance XReturnType          PTE = PtePlaceHolder
type instance XType                PTE = PtePlaceHolder
type instance XRefType             PTE = PtePlaceHolder
type instance XClassType           PTE = PtePlaceHolder
type instance XTypeArgument        PTE = PtePlaceHolder
type instance XNonWildTypeArgument PTE = PtePlaceHolder
type instance XWildcardBound       PTE = PtePlaceHolder
type instance XPrimType            PTE = PtePlaceHolder
type instance XTypeParam           PTE = PtePlaceHolder
type instance XPolicyExp           PTE = PtePlaceHolder
type instance XLockProperties      PTE = PtePlaceHolder
type instance XClause              PTE = PtePlaceHolder
type instance XClauseVarDecl       PTE = PtePlaceHolder
type instance XClauseHead          PTE = PtePlaceHolder
type instance XLClause             PTE = PtePlaceHolder
type instance XActor               PTE = PtePlaceHolder
type instance XActorName           PTE = PtePlaceHolder
type instance XAtom                PTE = PtePlaceHolder
type instance XLock                PTE = PtePlaceHolder
type instance XIdent               PTE = PtePlaceHolder