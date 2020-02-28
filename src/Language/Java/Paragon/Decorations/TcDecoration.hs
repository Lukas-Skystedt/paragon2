{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternSynonyms #-}

module Language.Java.Paragon.Decorations.TcDecoration where
import Language.Java.Paragon.Decorations.DecorationTypes
import Language.Java.Paragon.SourcePos
import Language.Java.Paragon.TypesTTG
import Language.Java.Paragon.SyntaxTTG
import Data.Void

data TC
type TcPlaceHolder = ()

-- data TcType = PrimT TcPrimT | RefT TcRefType

type instance XCompilationUnit     TC = NoFieldExt
type instance XPackageDecl         TC = NoFieldExt
type instance XImportDecl          TC = NoFieldExt
type instance XTypeDecl            TC = NoFieldExt
type instance XClassDecl           TC = NoFieldExt
type instance XClassBody           TC = NoFieldExt
type instance XEnumBody            TC = NoFieldExt
type instance XEnumConstant        TC = NoFieldExt
type instance XInterfaceDecl       TC = NoFieldExt
type instance XInterfaceBody       TC = NoFieldExt
type instance XDecl                TC = NoFieldExt
type instance XMemberDecl          TC = TcPlaceHolder
type instance XVarDecl             TC = TcPlaceHolder
type instance XVarDeclId           TC = TcPlaceHolder
-- type instance XInitExp             TC = TcPlaceHolder
type instance XFormalParam         TC = TcPlaceHolder
type instance XMethodBody          TC = TcPlaceHolder
type instance XConstructorBody     TC = TcPlaceHolder
type instance XExplConstrInv       TC = TcPlaceHolder
type instance XMod                 TC = TcPlaceHolder
type instance XBlock               TC = TcPlaceHolder
type instance XBlockStm            TC = TcPlaceHolder
type instance XStm                 TC = TcPlaceHolder
type instance XCatch               TC = TcPlaceHolder
type instance XSwitchBlock         TC = TcPlaceHolder
type instance XSwitchLabel         TC = TcPlaceHolder
type instance XForInit             TC = TcPlaceHolder
type instance XExceptionSpec       TC = TcPlaceHolder
type instance XExp                 TC = TcPlaceHolder
type instance XLiteral             TC = TcPlaceHolder
type instance XOp                  TC = TcPlaceHolder
type instance XAssignOp            TC = TcPlaceHolder
type instance XLhs                 TC = TcPlaceHolder
type instance XArrayIndex          TC = TcPlaceHolder
type instance XFieldAccess         TC = TcPlaceHolder
type instance XMethodInvocation    TC = TcPlaceHolder
type instance XArrayInit           TC = TcPlaceHolder
type instance XReturnType          TC = TcPlaceHolder
type instance XType                TC = TcPlaceHolder
type instance XRefType             TC = TcPlaceHolder
type instance XClassType           TC = TcPlaceHolder
type instance XTypeArgument        TC = TcPlaceHolder
type instance XNonWildTypeArgument TC = TcPlaceHolder
type instance XWildcardBound       TC = TcPlaceHolder
type instance XPrimType            TC = TcPlaceHolder
type instance XTypeParam           TC = TcPlaceHolder
type instance XPolicyExp           TC = TcPlaceHolder
type instance XLockProperties      TC = TcPlaceHolder
type instance XClause              TC = TcPlaceHolder
type instance XClauseVarDecl       TC = TcPlaceHolder
type instance XClauseHead          TC = TcPlaceHolder
type instance XLClause             TC = TcPlaceHolder
type instance XActor               TC = TcPlaceHolder
type instance XActorName           TC = TcPlaceHolder
type instance XAtom                TC = TcPlaceHolder
type instance XLock                TC = TcPlaceHolder
type instance XIdent               TC = TcPlaceHolder
