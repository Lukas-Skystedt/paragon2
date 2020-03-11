{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Java.Paragon.Decorations.PcsDecoration where
import Language.Java.Paragon.Decorations.DecorationTypes
import Language.Java.Paragon.SourcePos
import Language.Java.Paragon.TypesTTG
import Language.Java.Paragon.SyntaxTTG
import Data.Void

data PCS deriving Eq
type PcsPlaceHolder = ()

type instance XCompilationUnit     PCS = NoFieldExt
type instance XPackageDecl         PCS = NoFieldExt
type instance XImportDecl          PCS = NoFieldExt
type instance XTypeDecl            PCS = NoFieldExt
type instance XClassDecl           PCS = NoFieldExt
type instance XClassBody           PCS = NoFieldExt
type instance XEnumBody            PCS = NoFieldExt
type instance XEnumConstant        PCS = NoFieldExt
type instance XInterfaceDecl       PCS = NoFieldExt
type instance XInterfaceBody       PCS = NoFieldExt
type instance XDecl                PCS = NoFieldExt
type instance XMemberDecl          PCS = NoFieldExt
type instance XVarDecl             PCS = NoFieldExt
type instance XVarDeclId           PCS = NoFieldExt
type instance XFormalParam         PCS = NoFieldExt
type instance XMethodBody          PCS = NoFieldExt
type instance XConstructorBody     PCS = NoFieldExt
type instance XExplConstrInv       PCS = NoFieldExt
type instance XMod                 PCS = NoFieldExt
type instance XBlock               PCS = NoFieldExt
type instance XBlockStm            PCS = NoFieldExt
type instance XStm                 PCS = NoFieldExt
type instance XCatch               PCS = NoFieldExt
type instance XSwitchBlock         PCS = NoFieldExt
type instance XSwitchLabel         PCS = NoFieldExt
type instance XForInit             PCS = NoFieldExt
type instance XExceptionSpec       PCS = NoFieldExt
type instance XExp                 PCS = NoFieldExt
type instance XLiteral             PCS = NoFieldExt
type instance XOp                  PCS = NoFieldExt
type instance XAssignOp            PCS = NoFieldExt
type instance XLhs                 PCS = NoFieldExt
type instance XArrayIndex          PCS = NoFieldExt
type instance XFieldAccess         PCS = NoFieldExt
type instance XMethodInvocation    PCS = NoFieldExt
type instance XArrayInit           PCS = NoFieldExt
type instance XReturnType          PCS = NoFieldExt
type instance XType                PCS = NoFieldExt
type instance XRefType             PCS = NoFieldExt
type instance XClassType           PCS = NoFieldExt
type instance XTypeArgument        PCS = NoFieldExt
type instance XNonWildTypeArgument PCS = NoFieldExt
type instance XWildcardBound       PCS = NoFieldExt
type instance XPrimType            PCS = NoFieldExt
type instance XTypeParam           PCS = NoFieldExt
type instance XPolicyExp           PCS = NoFieldExt
type instance XLockProperties      PCS = NoFieldExt
type instance XClause              PCS = NoFieldExt
type instance XClauseVarDecl       PCS = NoFieldExt
type instance XClauseHead          PCS = NoFieldExt
type instance XLClause             PCS = NoFieldExt
type instance XActor               PCS = NoFieldExt
type instance XActorName           PCS = NoFieldExt
type instance XAtom                PCS = NoFieldExt
type instance XLock                PCS = NoFieldExt
type instance XIdent               PCS = NoFieldExt
type instance XName                PCS = NoFieldExt
type instance XVarInit             PCS = NoFieldExt

$(makePatternSyns
  "Pcs"
  [ 'TypeParam, 'CompilationUnit, 'PackageDecl, 'SingleTypeImport
  , 'TypeImportOnDemand , 'SingleStaticImport, 'StaticImportOnDemand
  , 'ClassTypeDecl, 'InterfaceTypeDecl , 'ClassDecl, 'EnumDecl, 'ClassBody
  , 'EnumBody, 'EnumConstant, 'InterfaceDecl , 'InterfaceBody, 'MemberDecl
  , 'InitDecl, 'FieldDecl, 'MethodDecl, 'ConstructorDecl , 'MemberClassDecl
  , 'MemberInterfaceDecl, 'LockDecl, 'VarDecl, 'VarId , 'VarDeclArray, 'InitExp
  , 'InitArray, 'FormalParam, 'MethodBody, 'ConstructorBody , 'ThisInvoke
  , 'SuperInvoke, 'PrimarySuperInvoke, 'Public, 'Private, 'Protected
  , 'Abstract, 'Final, 'Static, 'StrictFP, 'Transient, 'Volatile, 'Native
  , 'Typemethod , 'Reflexive, 'Transitive, 'Symmetric, 'Readonly, 'Notnull
  , 'Reads, 'Writes, 'Opens , 'Closes, 'Expects, 'Block, 'BlockStmt
  , 'LocalClass, 'LocalVars, 'LocalLock , 'StmtBlock, 'IfThen, 'IfThenElse
  , 'While, 'BasicFor, 'EnhancedFor, 'Empty, 'ExpStmt , 'Assert, 'Switch, 'Do
  , 'Break, 'Continue, 'Return, 'Synchronized, 'Throw, 'Try , 'Labeled, 'Open
  , 'Close, 'OpenBlock, 'CloseBlock, 'Catch, 'SwitchBlock, 'SwitchCase
  , 'Default, 'ForLocalVars, 'ForInitExps, 'ExceptionSpec, 'Lit, 'ClassLit
  , 'This , 'ThisClass, 'Paren, 'InstanceCreation, 'QualInstanceCreation
  , 'ArrayCreate , 'ArrayCreateInit, 'FieldAccess, 'MethodInv, 'ArrayAccess
  , 'ExpName , 'PostIncrement, 'PostDecrement, 'PreIncrement, 'PreDecrement
  , 'PrePlus, 'PreMinus , 'PreBitCompl, 'PreNot, 'Cast, 'BinOp, 'InstanceOf
  , 'Cond, 'Assign, 'PolicyExp , 'LockExp, 'Int, 'Word, 'Float, 'Double
  , 'Boolean, 'Char, 'String, 'Null, 'Mult, 'Div , 'Rem, 'Add, 'Sub, 'LShift
  , 'RShift, 'RRShift, 'LThan, 'GThan, 'LThanE, 'GThanE, 'Equal , 'NotEq, 'And
  , 'Or, 'Xor, 'CAnd, 'COr, 'EqualA, 'MultA, 'DivA, 'RemA, 'AddA, 'SubA
  , 'LShiftA, 'RShiftA, 'RRShiftA, 'AndA, 'XorA, 'OrA, 'NameLhs, 'FieldLhs
  , 'ArrayLhs , 'ArrayIndex, 'PrimaryFieldAccess, 'SuperFieldAccess
  , 'ClassFieldAccess , 'MethodCallOrLockQuery, 'PrimaryMethodCall
  , 'SuperMethodCall, 'ClassMethodCall , 'TypeMethodCall, 'ArrayInit, 'VoidType
  , 'LockType, 'Type, 'PrimType, 'RefType , 'ClassRefType, 'TypeVariable
  , 'ArrayType, 'ClassType, 'Wildcard, 'ActualArg , 'ActualName, 'ActualType
  , 'ActualExp, 'ActualLockState, 'ExtendsBound , 'SuperBound, 'BooleanT
  , 'ByteT, 'ShortT, 'IntT, 'LongT, 'CharT, 'FloatT, 'DoubleT , 'ActorT
  , 'PolicyT, 'ActorParam, 'PolicyParam, 'LockStateParam, 'PolicyLit
  , 'PolicyOf, 'PolicyThis, 'PolicyTypeVar, 'LockProperties, 'Clause
  , 'ClauseVarDecl , 'ClauseDeclHead, 'ClauseVarHead, 'LClause
  , 'ConstraintClause, 'Actor, 'Var , 'ActorName, 'ActorTypeVar, 'Atom, 'Lock
  , 'LockVar, 'Ident
  ]
 [p| () |]
 )
