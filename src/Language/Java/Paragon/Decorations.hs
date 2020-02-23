{-# LANGUAGE TypeFamilies #-}
module Decorations where
import Language.Java.Paragon.SourcePos
import Data.Void
-- import Language.Java.Paragon.SyntaxTTG

-- | Data type for when an extension field is not used. (As in Trees That Grow)
type NoFieldExt = ()
-- | Data type for when a extension constructor is not used. (As in Trees That Grow)
type NoConExt = Void

--------------------------------------------------------------------------------
-- Parser
--------------------------------------------------------------------------------
data Parser

type instance XSP                  Parser = SourcePos
type instance XCompilationUnit     Parser = NoFieldExt
type instance XPackageDecl         Parser = NoFieldExt
type instance XImportDecl          Parser = NoFieldExt
type instance XTypeDecl            Parser = NoFieldExt
type instance XClassDecl           Parser = NoFieldExt
type instance XClassBody           Parser = NoFieldExt
type instance XEnumBody            Parser = NoFieldExt
type instance XEnumConstant        Parser = NoFieldExt
type instance XInterfaceDecl       Parser = NoFieldExt
type instance XInterfaceBody       Parser = NoFieldExt
type instance XDecl                Parser = NoFieldExt
type instance XMemberDecl          Parser = NoFieldExt
type instance XVarDecl             Parser = NoFieldExt
type instance XVarDeclId           Parser = NoFieldExt
type instance XInitExp             Parser = NoFieldExt
type instance XFormalParam         Parser = NoFieldExt
type instance XMethodBody          Parser = NoFieldExt
type instance XConstructorBody     Parser = NoFieldExt
type instance XExplConstrInv       Parser = NoFieldExt
type instance XMod                 Parser = NoFieldExt
type instance XBlock               Parser = NoFieldExt
type instance XBlockStm            Parser = NoFieldExt
type instance XStm                 Parser = NoFieldExt
type instance XCatch               Parser = NoFieldExt
type instance XSwitchBlock         Parser = NoFieldExt
type instance XSwitchLabel         Parser = NoFieldExt
type instance XForInit             Parser = NoFieldExt
type instance XExceptionSpec       Parser = NoFieldExt
type instance XExp                 Parser = NoFieldExt
type instance XLiteral             Parser = NoFieldExt
type instance XOp                  Parser = NoFieldExt
type instance XAssignOp            Parser = NoFieldExt
type instance XLhs                 Parser = NoFieldExt
type instance XArrayIndex          Parser = NoFieldExt
type instance XFieldAccess         Parser = NoFieldExt
type instance XMethodInvocation    Parser = NoFieldExt
type instance XArrayInit           Parser = NoFieldExt
type instance XReturnType          Parser = NoFieldExt
type instance XType                Parser = NoFieldExt
type instance XRefType             Parser = NoFieldExt
type instance XClassType           Parser = NoFieldExt
type instance XTypeArgument        Parser = NoFieldExt
type instance XNonWildTypeArgument Parser = NoFieldExt
type instance XWildcardBound       Parser = NoFieldExt
type instance XPrimType            Parser = NoFieldExt
type instance XTypeParam           Parser = NoFieldExt
type instance XPolicyExp           Parser = NoFieldExt
type instance XLockProperties      Parser = NoFieldExt
type instance XClause              Parser = NoFieldExt
type instance XClauseVarDecl       Parser = NoFieldExt
type instance XClauseHead          Parser = NoFieldExt
type instance XLClause             Parser = NoFieldExt
type instance XActor               Parser = NoFieldExt
type instance XActorName           Parser = NoFieldExt
type instance XAtom                Parser = NoFieldExt
type instance XLock                Parser = NoFieldExt
type instance XIdent               Parser = NoFieldExt
type instance XName                Parser = NoFieldExt

--------------------------------------------------------------------------------
-- NameResolution (NR)
--------------------------------------------------------------------------------
data NR

type instance XSP                  NR = SourcePos
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
type instance XInitExp             NR = NoFieldExt
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
type instance XName                NR = NoFieldExt

--------------------------------------------------------------------------------
-- TypeCheck (TC)
--------------------------------------------------------------------------------

data TC
type TcPlaceHolder = ()

type instance XSP                  TC = SourcePos
type instance XCompilationUnit     TC = TcPlaceHolder
type instance XPackageDecl         TC = TcPlaceHolder
type instance XImportDecl          TC = TcPlaceHolder
type instance XTypeDecl            TC = TcPlaceHolder
type instance XClassDecl           TC = TcPlaceHolder
type instance XClassBody           TC = TcPlaceHolder
type instance XEnumBody            TC = TcPlaceHolder
type instance XEnumConstant        TC = TcPlaceHolder
type instance XInterfaceDecl       TC = TcPlaceHolder
type instance XInterfaceBody       TC = TcPlaceHolder
type instance XDecl                TC = TcPlaceHolder
type instance XMemberDecl          TC = TcPlaceHolder
type instance XVarDecl             TC = TcPlaceHolder
type instance XVarDeclId           TC = TcPlaceHolder
type instance XInitExp             TC = TcPlaceHolder
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
type instance XName                TC = TcPlaceHolder

--------------------------------------------------------------------------------
-- PolicyTypeEval (PTE)
--------------------------------------------------------------------------------

data PTE
type PtePlaceHolder = ()

type instance XSP                  PTE = SourcePos
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
type instance XInitExp             PTE = PtePlaceHolder
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
type instance XName                PTE = PtePlaceHolder
--------------------------------------------------------------------------------
-- LockStateEval (LSE)
--------------------------------------------------------------------------------

data LSE
type LsePlaceHolder = ()

type instance XSP                  LSE = SourcePos
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
type instance XInitExp             LSE = LsePlaceHolder
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
type instance XName                LSE = LsePlaceHolder

--------------------------------------------------------------------------------
-- PolicyConstraintGen (PCG)
--------------------------------------------------------------------------------

data PCG
type PcgPlaceHolder = ()

type instance XSP                  PCG = SourcePos
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
type instance XInitExp             PCG = PcgPlaceHolder
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
type instance XName                PCG = PcgPlaceHolder

--------------------------------------------------------------------------------
-- PolicyConstraintSolver (PCS)
--------------------------------------------------------------------------------

data PCS
type PcsPlaceHolder = ()

type instance XSP                  PCS = SourcePos
type instance XCompilationUnit     PCS = PcsPlaceHolder
type instance XPackageDecl         PCS = PcsPlaceHolder
type instance XImportDecl          PCS = PcsPlaceHolder
type instance XTypeDecl            PCS = PcsPlaceHolder
type instance XClassDecl           PCS = PcsPlaceHolder
type instance XClassBody           PCS = PcsPlaceHolder
type instance XEnumBody            PCS = PcsPlaceHolder
type instance XEnumConstant        PCS = PcsPlaceHolder
type instance XInterfaceDecl       PCS = PcsPlaceHolder
type instance XInterfaceBody       PCS = PcsPlaceHolder
type instance XDecl                PCS = PcsPlaceHolder
type instance XMemberDecl          PCS = PcsPlaceHolder
type instance XVarDecl             PCS = PcsPlaceHolder
type instance XVarDeclId           PCS = PcsPlaceHolder
type instance XInitExp             PCS = PcsPlaceHolder
type instance XFormalParam         PCS = PcsPlaceHolder
type instance XMethodBody          PCS = PcsPlaceHolder
type instance XConstructorBody     PCS = PcsPlaceHolder
type instance XExplConstrInv       PCS = PcsPlaceHolder
type instance XMod                 PCS = PcsPlaceHolder
type instance XBlock               PCS = PcsPlaceHolder
type instance XBlockStm            PCS = PcsPlaceHolder
type instance XStm                 PCS = PcsPlaceHolder
type instance XCatch               PCS = PcsPlaceHolder
type instance XSwitchBlock         PCS = PcsPlaceHolder
type instance XSwitchLabel         PCS = PcsPlaceHolder
type instance XForInit             PCS = PcsPlaceHolder
type instance XExceptionSpec       PCS = PcsPlaceHolder
type instance XExp                 PCS = PcsPlaceHolder
type instance XLiteral             PCS = PcsPlaceHolder
type instance XOp                  PCS = PcsPlaceHolder
type instance XAssignOp            PCS = PcsPlaceHolder
type instance XLhs                 PCS = PcsPlaceHolder
type instance XArrayIndex          PCS = PcsPlaceHolder
type instance XFieldAccess         PCS = PcsPlaceHolder
type instance XMethodInvocation    PCS = PcsPlaceHolder
type instance XArrayInit           PCS = PcsPlaceHolder
type instance XReturnType          PCS = PcsPlaceHolder
type instance XType                PCS = PcsPlaceHolder
type instance XRefType             PCS = PcsPlaceHolder
type instance XClassType           PCS = PcsPlaceHolder
type instance XTypeArgument        PCS = PcsPlaceHolder
type instance XNonWildTypeArgument PCS = PcsPlaceHolder
type instance XWildcardBound       PCS = PcsPlaceHolder
type instance XPrimType            PCS = PcsPlaceHolder
type instance XTypeParam           PCS = PcsPlaceHolder
type instance XPolicyExp           PCS = PcsPlaceHolder
type instance XLockProperties      PCS = PcsPlaceHolder
type instance XClause              PCS = PcsPlaceHolder
type instance XClauseVarDecl       PCS = PcsPlaceHolder
type instance XClauseHead          PCS = PcsPlaceHolder
type instance XLClause             PCS = PcsPlaceHolder
type instance XActor               PCS = PcsPlaceHolder
type instance XActorName           PCS = PcsPlaceHolder
type instance XAtom                PCS = PcsPlaceHolder
type instance XLock                PCS = PcsPlaceHolder
type instance XIdent               PCS = PcsPlaceHolder
type instance XName                PCS = PcsPlaceHolder


--------------------------------------------------------------------------------
-- Since the new Syntax file does not compile, the type families are replicated
-- here for now.
--------------------------------------------------------------------------------
type family XSP                  x
type family XCompilationUnit     x
type family XPackageDecl         x
type family XImportDecl          x
type family XTypeDecl            x
type family XClassDecl           x
type family XClassBody           x
type family XEnumBody            x
type family XEnumConstant        x
type family XInterfaceDecl       x
type family XInterfaceBody       x
type family XDecl                x
type family XMemberDecl          x
type family XVarDecl             x
type family XVarDeclId           x
type family XInitExp             x
type family XFormalParam         x
type family XMethodBody          x
type family XConstructorBody     x
type family XExplConstrInv       x
type family XMod                 x
type family XBlock               x
type family XBlockStm            x
type family XStm                 x
type family XCatch               x
type family XSwitchBlock         x
type family XSwitchLabel         x
type family XForInit             x
type family XExceptionSpec       x
type family XExp                 x
type family XLiteral             x
type family XOp                  x
type family XAssignOp            x
type family XLhs                 x
type family XArrayIndex          x
type family XFieldAccess         x
type family XMethodInvocation    x
type family XArrayInit           x
type family XReturnType          x
type family XType                x
type family XRefType             x
type family XClassType           x
type family XTypeArgument        x
type family XNonWildTypeArgument x
type family XWildcardBound       x
type family XPrimType            x
type family XTypeParam           x
type family XPolicyExp           x
type family XLockProperties      x
type family XClause              x
type family XClauseVarDecl       x
type family XClauseHead          x
type family XLClause             x
type family XActor               x
type family XActorName           x
type family XAtom                x
type family XLock                x
type family XIdent               x
type family XName                x
