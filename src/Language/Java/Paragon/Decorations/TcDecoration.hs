{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternSynonyms #-}

module Language.Java.Paragon.Decorations.TcDecoration where
import Language.Java.Paragon.Decorations.DecorationTypes
import Language.Java.Paragon.SourcePos
import Language.Java.Paragon.TypesTTG
import Language.Java.Paragon.SyntaxTTG
import Data.Void

data Tc
type TcPlaceHolder = ()

-- data TcType = PrimT TcPrimT | RefT TcRefType

type instance XCompilationUnit     Tc = SourcePos
type instance XPackageDecl         Tc = SourcePos
type instance XImportDecl          Tc = SourcePos
type instance XTypeDecl            Tc = SourcePos
type instance XClassDecl           Tc = SourcePos
type instance XClassBody           Tc = SourcePos
type instance XEnumBody            Tc = SourcePos
type instance XEnumConstant        Tc = SourcePos
type instance XInterfaceDecl       Tc = SourcePos
type instance XInterfaceBody       Tc = SourcePos
type instance XDecl                Tc = SourcePos
type instance XMemberDecl          Tc = (SourcePos, TcPlaceHolder)
type instance XVarDecl             Tc = (SourcePos, TcPlaceHolder)
type instance XVarDeclId           Tc = (SourcePos, TcPlaceHolder)
type instance XFormalParam         Tc = (SourcePos, TcPlaceHolder)
type instance XMethodBody          Tc = (SourcePos, TcPlaceHolder)
type instance XConstructorBody     Tc = (SourcePos, TcPlaceHolder)
type instance XExplConstrInv       Tc = (SourcePos, TcPlaceHolder)
type instance XMod                 Tc = (SourcePos, TcPlaceHolder)
type instance XBlock               Tc = (SourcePos, TcPlaceHolder)
type instance XBlockStm            Tc = (SourcePos, TcPlaceHolder)
type instance XStm                 Tc = (SourcePos, TcPlaceHolder)
type instance XCatch               Tc = (SourcePos, TcPlaceHolder)
type instance XSwitchBlock         Tc = (SourcePos, TcPlaceHolder)
type instance XSwitchLabel         Tc = (SourcePos, TcPlaceHolder)
type instance XForInit             Tc = (SourcePos, TcPlaceHolder)
type instance XExceptionSpec       Tc = (SourcePos, TcPlaceHolder)
type instance XExp                 Tc = (SourcePos, TcPlaceHolder)
type instance XLiteral             Tc = (SourcePos, TcPlaceHolder)
type instance XOp                  Tc = (SourcePos, TcPlaceHolder)
type instance XAssignOp            Tc = (SourcePos, TcPlaceHolder)
type instance XLhs                 Tc = (SourcePos, TcPlaceHolder)
type instance XArrayIndex          Tc = (SourcePos, TcPlaceHolder)
type instance XFieldAccess         Tc = (SourcePos, TcPlaceHolder)
type instance XMethodInvocation    Tc = (SourcePos, TcPlaceHolder)
type instance XArrayInit           Tc = (SourcePos, TcPlaceHolder)
type instance XReturnType          Tc = (SourcePos, TcPlaceHolder)
type instance XType                Tc = (SourcePos, TcPlaceHolder)
type instance XRefType             Tc = (SourcePos, TcPlaceHolder)
type instance XClassType           Tc = (SourcePos, TcPlaceHolder)
type instance XTypeArgument        Tc = (SourcePos, TcPlaceHolder)
type instance XNonWildTypeArgument Tc = (SourcePos, TcPlaceHolder)
type instance XWildcardBound       Tc = (SourcePos, TcPlaceHolder)
type instance XPrimType            Tc = (SourcePos, TcPlaceHolder)
type instance XTypeParam           Tc = (SourcePos, TcPlaceHolder)
type instance XPolicyExp           Tc = (SourcePos, TcPlaceHolder)
type instance XLockProperties      Tc = (SourcePos, TcPlaceHolder)
type instance XClause              Tc = (SourcePos, TcPlaceHolder)
type instance XClauseVarDecl       Tc = (SourcePos, TcPlaceHolder)
type instance XClauseHead          Tc = (SourcePos, TcPlaceHolder)
type instance XLClause             Tc = (SourcePos, TcPlaceHolder)
type instance XActor               Tc = (SourcePos, TcPlaceHolder)
type instance XActorName           Tc = (SourcePos, TcPlaceHolder)
type instance XAtom                Tc = (SourcePos, TcPlaceHolder)
type instance XLock                Tc = (SourcePos, TcPlaceHolder)
type instance XIdent               Tc = (SourcePos, TcPlaceHolder)

