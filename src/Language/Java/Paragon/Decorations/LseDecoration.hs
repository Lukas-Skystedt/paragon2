{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternSynonyms #-}

module Language.Java.Paragon.Decorations.LseDecoration where
import Language.Java.Paragon.Decorations.DecorationTypes
import Language.Java.Paragon.SourcePos
import Language.Java.Paragon.TypesTTG
import Language.Java.Paragon.SyntaxTTG
import Data.Void

data LSE
type LsePlaceHolder = ()

type instance XCompilationUnit     LSE = LsePlaceHolder
type instance XPackageDecl         LSE = LsePlaceHolder
type instance XImportDecl          LSE = LsePlaceHolder
type instance XTypeDecl            LSE = LsePlaceHolder
type instance XClassDecl           LSE = LsePlaceHolder
type instance XClassBody           LSE = LsePlaceHolder
type instance XEnumBody            LSE = LsePlaceHolder
type instance XEnumConstant        LSE = LsePlaceHolder
type instance XInterfaceDecl       LSE = LsePlaceHolder
type instance XInterfaceBody       LSE = LsePlaceHolder
type instance XDecl                LSE = LsePlaceHolder
type instance XMemberDecl          LSE = LsePlaceHolder
type instance XVarDecl             LSE = LsePlaceHolder
type instance XVarDeclId           LSE = LsePlaceHolder
-- type instance XInitExp             LSE = LsePlaceHolder
type instance XFormalParam         LSE = LsePlaceHolder
type instance XMethodBody          LSE = LsePlaceHolder
type instance XConstructorBody     LSE = LsePlaceHolder
type instance XExplConstrInv       LSE = LsePlaceHolder
type instance XMod                 LSE = LsePlaceHolder
type instance XBlock               LSE = LsePlaceHolder
type instance XBlockStm            LSE = LsePlaceHolder
type instance XStm                 LSE = LsePlaceHolder
type instance XCatch               LSE = LsePlaceHolder
type instance XSwitchBlock         LSE = LsePlaceHolder
type instance XSwitchLabel         LSE = LsePlaceHolder
type instance XForInit             LSE = LsePlaceHolder
type instance XExceptionSpec       LSE = LsePlaceHolder
type instance XExp                 LSE = LsePlaceHolder
type instance XLiteral             LSE = LsePlaceHolder
type instance XOp                  LSE = LsePlaceHolder
type instance XAssignOp            LSE = LsePlaceHolder
type instance XLhs                 LSE = LsePlaceHolder
type instance XArrayIndex          LSE = LsePlaceHolder
type instance XFieldAccess         LSE = LsePlaceHolder
type instance XMethodInvocation    LSE = LsePlaceHolder
type instance XArrayInit           LSE = LsePlaceHolder
type instance XReturnType          LSE = LsePlaceHolder
type instance XType                LSE = LsePlaceHolder
type instance XRefType             LSE = LsePlaceHolder
type instance XClassType           LSE = LsePlaceHolder
type instance XTypeArgument        LSE = LsePlaceHolder
type instance XNonWildTypeArgument LSE = LsePlaceHolder
type instance XWildcardBound       LSE = LsePlaceHolder
type instance XPrimType            LSE = LsePlaceHolder
type instance XTypeParam           LSE = LsePlaceHolder
type instance XPolicyExp           LSE = LsePlaceHolder
type instance XLockProperties      LSE = LsePlaceHolder
type instance XClause              LSE = LsePlaceHolder
type instance XClauseVarDecl       LSE = LsePlaceHolder
type instance XClauseHead          LSE = LsePlaceHolder
type instance XLClause             LSE = LsePlaceHolder
type instance XActor               LSE = LsePlaceHolder
type instance XActorName           LSE = LsePlaceHolder
type instance XAtom                LSE = LsePlaceHolder
type instance XLock                LSE = LsePlaceHolder
type instance XIdent               LSE = LsePlaceHolder

