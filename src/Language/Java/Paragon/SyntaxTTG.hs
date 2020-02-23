{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE CPP, DeriveDataTypeable, DeriveFunctor, TemplateHaskell,
             FlexibleContexts, MultiParamTypeClasses, FlexibleInstances #-}

{-# LANGUAGE TypeFamilies, DataKinds, ConstraintKinds #-}
{-# LANGUAGE GADTs, EmptyCase, StandaloneDeriving #-}
{-# LANGUAGE TypeOperators, PatternSynonyms #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
module Language.Java.Paragon.SyntaxTTG (
    module Language.Java.Paragon.SyntaxTTG,
    module Language.Java.Paragon.Annotated
                                    ) where

import Data.Data

import Language.Java.Paragon.Annotated
import Language.Java.Paragon.Interaction
import Language.Java.Paragon.SourcePos
import GHC.Types (Constraint)
import qualified Data.ByteString.Char8 as B

syntaxModule :: String
syntaxModule = libraryBase ++ ".Syntax"


-----------------------------------------------------------------------
-- Packages
type family XSP x

-- | A compilation unit is the top level syntactic goal symbol of a Java program
-- This usually corresponds to a single .java source file that may start with
-- a package declaration, followed by a (possibly empty) list of imports and
-- a (usually non-empty) list of type declaration, where a type is a class
-- or interface.
-- Note that the paragon compiler currently only accepts a single type per
-- compilation unit and does not yet suport enums, although files containing
-- enums can be parsed
data CompilationUnit x = CompilationUnit (XCompilationUnit x) (XSP x) (Maybe (PackageDecl x)) [ImportDecl x] [TypeDecl x]
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Ord,Show,Typeable,Data,Functor)
#else
  deriving (Eq,Ord,Show)
#endif
type family XCompilationUnit x

-- | A package declaration appears within a compilation unit to indicate the package to which the compilation unit belongs.
data PackageDecl x = PackageDecl (XPackageDecl x) (XSP x) (Name x)
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Ord,Show,Typeable,Data,Functor)
#else
  deriving (Eq,Ord,Show)
#endif
type family XPackageDecl x

-- | An import declaration allows a static member or a named type to be referred
-- to by a single unqualified identifier.
-- The first argument signals whether the declaration only imports static
-- members.
-- The last argument signals whether the declaration brings all names in the
-- named type or package, or only brings a single name into scope.
data ImportDecl x
    = SingleTypeImport     (XImportDecl x) (XSP x) (Name x)
      -- ^Import a single type (class/interface/enum)
    | TypeImportOnDemand   (XImportDecl x) (XSP x) (Name x)
      -- ^Bring all types of package into scope, e.g. import java.lang.util.*
    | SingleStaticImport   (XImportDecl x) (XSP x) (Name x) (Ident x)
      -- ^Single static import, e.g. import static java.lang.Math.PI
    | StaticImportOnDemand (XImportDecl x) (XSP x) (Name x)
      -- ^Static import of all members, e.g. import static java.lang.Math.*
  deriving (Eq,Ord,Show,Typeable,Data,Functor)
type family XImportDecl x

-----------------------------------------------------------------------
-- Declarations

-- | A type declaration declares a class type or an interface type.
data TypeDecl x
    = ClassTypeDecl (XTypeDecl x) (XSP x) (ClassDecl x)
    | InterfaceTypeDecl (XTypeDecl x) (XSP x) (InterfaceDecl x)
  deriving (Eq,Ord,Show,Typeable,Data,Functor)
type family XTypeDecl x

-- | A class declaration specifies a new named reference type.
-- Note that the compiler does not actually deal with enums yet!
data ClassDecl x
    = ClassDecl (XClassDecl x) (XSP x) [Modifier x] (Ident x) [TypeParam x] (Maybe (ClassType x)) [ClassType x] (ClassBody x)
      -- ^Fields: Class modifiers, class identifier, type params, super class,
      -- if any, list of implemented interfaces, class body
    | EnumDecl  (XClassDecl x) (XSP x) [Modifier x] (Ident x)                                     [ClassType x] (EnumBody x)
  deriving (Eq,Ord,Show,Typeable,Data,Functor)
type family XClassDecl x

-- | A class body may contain declarations of members of the class, that is,
--   fields, classes, interfaces and methods.
--   A class body may also contain instance initializers, static
--   initializers, and declarations of constructors for the class.
data ClassBody x = ClassBody (XClassBody x) (XSP x) [Decl x]
  deriving (Eq,Ord,Show,Typeable,Data,Functor)
type family XClassBody x

-- | The body of an enum type may contain enum constants.
data EnumBody x = EnumBody (XEnumBody x) (XSP x) [EnumConstant x] [Decl x]
  deriving (Eq,Ord,Show,Typeable,Data,Functor)
type family XEnumBody x

-- | An enum constant defines an instance of the enum type.
data EnumConstant x = EnumConstant (XEnumConstant x) (XSP x) (Ident x) [Argument x] (Maybe (ClassBody x))
  deriving (Eq,Ord,Show,Typeable,Data,Functor)
type family XEnumConstant x

-- | An interface declaration introduces a new reference type whose members
--   are classes, interfaces, constants and abstract methods. This type has
--   no implementation, but otherwise unrelated classes can implement it by
--   providing implementations for its abstract methods.
data InterfaceDecl x
    = InterfaceDecl (XInterfaceDecl x) (XSP x) [Modifier x] (Ident x) [TypeParam x] [ClassType x] (InterfaceBody x)
  deriving (Eq,Ord,Show,Typeable,Data,Functor)
type family XInterfaceDecl x

-- | The body of an interface may declare members of the interface.
data InterfaceBody x
    = InterfaceBody (XInterfaceBody x) (XSP x) [MemberDecl x]
  deriving (Eq,Ord,Show,Typeable,Data,Functor)
type family XInterfaceBody x

-- | A declaration is either a member declaration, or a declaration of an
--   initializer, which may be static.
data Decl x
    = MemberDecl (XDecl x) (XSP x) (MemberDecl x)
    | InitDecl (XDecl x) (XSP x) Bool (Block x)
  deriving (Eq,Ord,Show,Typeable,Data,Functor)
type family XDecl x


-- | A class or interface member can be an inner class or interface, a field or
--   constant, or a method or constructor. An interface may only have as members
--   constants (not fields), abstract methods, and no constructors.
data MemberDecl x
    -- | The variables of a class type are introduced by field declarations.
    = FieldDecl (XMemberDecl x) (XSP x) [Modifier x] (Type x) [VarDecl x]
    -- | A method declares executable code that can be invoked, passing a fixed number of values as arguments.
    | MethodDecl (XMemberDecl x) (XSP x) [Modifier x] [TypeParam x] (ReturnType x) (Ident x) [FormalParam x] [ExceptionSpec x] (MethodBody x)
    -- | A constructor is used in the creation of an object that is an instance of a class.
    | ConstructorDecl (XMemberDecl x) (XSP x) [Modifier x] [TypeParam x]                  (Ident x) [FormalParam x] [ExceptionSpec x] (ConstructorBody x)
    -- | A member class is a class whose declaration is directly enclosed in another class or interface declaration.
    | MemberClassDecl (XMemberDecl x) (XSP x) (ClassDecl x)
    -- | A member interface is an interface whose declaration is directly enclosed in another class or interface declaration.
    | MemberInterfaceDecl (XMemberDecl x) (XSP x) (InterfaceDecl x)

-- Paragon
    -- | A lock declaration is a special kind of field declaration.
    | LockDecl (XMemberDecl x) (XSP x) [Modifier x]  (Ident x) [RefType x] (Maybe (LockProperties x))
{-    -- | A policy declaration - should be a field decl really.
    | PolicyDecl a [Modifier a] Ident Policy -}
{-    -- | An actor declaration is a special kind of field declaration.
    | ActorDecl [Modifier] Ident (Maybe VarInit) -}
  deriving (Eq,Ord,Show,Typeable,Data,Functor)
type family XMemberDecl x

-- int x; => VarDecl (VarId "x") Nothing
-- int x = 1; => VarDecl (VarId "x") (Just ...)
-- | A declaration of a variable, which may be explicitly initialized.
data VarDecl x
    = VarDecl (XVarDecl x) (XSP x) (VarDeclId x) (Maybe (VarInit x))
  deriving (Eq,Ord,Show,Typeable,Data,Functor)
type family XVarDecl x

-- | The name of a variable in a declaration, which may be an array.
data VarDeclId x
    = VarId (XVarDeclId x) (XSP x) (Ident x)
    | VarDeclArray (XVarDeclId x) (XSP x) (VarDeclId x)
    -- ^ Multi-dimensional arrays are represented by nested applications of 'VarDeclArray' (Deprecated)
  deriving (Eq,Ord,Show,Typeable,Data,Functor)
type family XVarDeclId x

getVarDeclId :: VarDeclId a -> Ident a
getVarDeclId (VarId _ ident) = ident
getVarDeclId (VarDeclArray _ varDeclId) = getVarDeclId varDeclId

-- | Explicit initializer for a variable declaration.
data VarInit x
    = InitExp (XInitExp x) (XSP x) (Exp x)
    | InitArray (XInitExp x) (XSP x) (ArrayInit x)
  deriving (Eq,Ord,Show,Typeable,Data,Functor)
type family XInitExp x

-- | A formal parameter in method declaration. The last parameter
--   for a given declaration may be marked as variable arity,
--   indicated by the boolean argument.
data FormalParam x = FormalParam (XFormalParam x) (XSP x) [Modifier x] (Type x) Bool (VarDeclId x)
  deriving (Eq,Ord,Show,Typeable,Data,Functor)
type family XFormalParam x

getFormalParamId :: FormalParam a -> Ident a
getFormalParamId (FormalParam _ _ _ _ varDeclId) = getVarDeclId varDeclId

-- | A method body is either a block of code that implements the method or simply a
--   semicolon, indicating the lack of an implementation (modelled by 'Nothing').
data MethodBody x = MethodBody (XMethodBody x) (XSP x) (Maybe (Block x))
  deriving (Eq,Ord,Show,Typeable,Data,Functor)
type family XMethodBody x

-- | The first statement of a constructor body may be an explicit invocation of
--   another constructor of the same class or of the direct superclass.
data ConstructorBody x = ConstructorBody (XConstructorBody x) (XSP x) (Maybe (ExplConstrInv x)) [BlockStmt x]
  deriving (Eq,Ord,Show,Typeable,Data,Functor)
type family XConstructorBody x

-- | An explicit constructor invocation invokes another constructor of the
--   same class, or a constructor of the direct superclass, which may
--   be qualified to explicitly specify the newly created object's immediately
--   enclosing instance.
data ExplConstrInv x
    = ThisInvoke         (XExplConstrInv x) (XSP x)         [NonWildTypeArgument x] [Argument x]
    | SuperInvoke        (XExplConstrInv x) (XSP x)         [NonWildTypeArgument x] [Argument x]
    | PrimarySuperInvoke (XExplConstrInv x) (XSP x) (Exp x) [NonWildTypeArgument x] [Argument x]
  deriving (Eq,Ord,Show,Typeable,Data,Functor)
type family XExplConstrInv x

-- | A modifier specifying properties of a given declaration. In general only
--   a few of these modifiers are allowed for each declaration type, for instance
--   a member type declaration may only specify one of public, private or protected.
data Modifier x
    = Public    (XMod x) (XSP x)
    | Private   (XMod x) (XSP x)
    | Protected (XMod x) (XSP x)
    | Abstract  (XMod x) (XSP x)
    | Final     (XMod x) (XSP x)
    | Static    (XMod x) (XSP x)
    | StrictFP  (XMod x) (XSP x)
    | Transient (XMod x) (XSP x)
    | Volatile  (XMod x) (XSP x)
    | Native    (XMod x) (XSP x)

    | Typemethod (XMod x) (XSP x)
    | Reflexive  (XMod x) (XSP x)
    | Transitive (XMod x) (XSP x)
    | Symmetric  (XMod x) (XSP x)
    | Readonly   (XMod x) (XSP x)
    | Notnull    (XMod x) (XSP x)

    | Reads   (XMod x) (XSP x) (Policy x)
    | Writes  (XMod x) (XSP x) (Policy x)
    | Opens   (XMod x) (XSP x) [Lock x]
    | Closes  (XMod x) (XSP x) [Lock x]
    | Expects (XMod x) (XSP x) [Lock x]
  deriving (Eq,Ord,Show,Typeable,Data,Functor)
type family XMod x

isMethodStatic :: [Modifier a] -> Bool
isMethodStatic ms = Static () `elem` removeAnnotationMany ms

-----------------------------------------------------------------------
-- Statements

-- | A block is a sequence of statements, local class declarations
--   and local variable declaration statements within braces.
data Block x = Block (XBlock x) (XSP x) [BlockStmt x]
  deriving (Eq,Ord,Show,Typeable,Data,Functor)
type family XBlock x


-- | A block statement is either a normal statement, a local
--   class declaration or a local variable declaration.
data BlockStmt x
    = BlockStmt (XBlockStm x) (XSP x) (Stmt x)
    | LocalClass (XBlockStm x) (XSP x) (ClassDecl x)
    | LocalVars (XBlockStm x) (XSP x) [Modifier x] (Type x) [VarDecl x]

-- Paragon
    | LocalLock (XBlockStm x) (XSP x) [Modifier x] (Ident x) [RefType x] (Maybe (LockProperties x))
{-    | LocalPolicy [Modifier] Ident Policy
      | LocalActor [Modifier] Ident (Maybe VarInit) -}
  deriving (Eq,Ord,Show,Typeable,Data,Functor)
type family XBlockStm x


-- | A Java statement.
data Stmt x
    -- | A statement can be a nested block.
    = StmtBlock (XStm x) (XSP x) (Block x)
    -- | The @if-then@ statement allows conditional execution of a statement.
    | IfThen (XStm x) (XSP x) (Exp x) (Stmt x)
    -- | The @if-then-else@ statement allows conditional choice of two statements, executing one or the other but not both.
    | IfThenElse (XStm x) (XSP x) (Exp x) (Stmt x) (Stmt x)
    -- | The @while@ statement executes an expression and a statement repeatedly until the value of the expression is false.
    | While (XStm x) (XSP x) (Exp x) (Stmt x)
    -- | The basic @for@ statement executes some initialization code, then executes an expression, a statement, and some
    --   update code repeatedly until the value of the expression is false.
    | BasicFor (XStm x) (XSP x) (Maybe (ForInit x)) (Maybe (Exp x)) (Maybe [Exp x]) (Stmt x)
    -- | The enhanced @for@ statement iterates over an array or a value of a class that implements the @iterator@ interface.
    | EnhancedFor (XStm x) (XSP x) [Modifier x] (Type x) (Ident x) (Exp x) (Stmt x)
    -- | An empty statement does nothing.
    | Empty (XStm x) (XSP x)
    -- | Certain kinds of expressions may be used as statements by following them with semicolons:
    --   assignments, pre- or post-inc- or decrementation, method invocation or class instance
    --   creation expressions.
    | ExpStmt (XStm x) (XSP x) (Exp x)
    -- | An assertion is a statement containing a boolean expression, where an error is reported if the expression
    --   evaluates to false.
    | Assert (XStm x) (XSP x) (Exp x) (Maybe (Exp x))
    -- | The switch statement transfers control to one of several statements depending on the value of an expression.
    | Switch (XStm x) (XSP x) (Exp x) [SwitchBlock x]
    -- | The @do@ statement executes a statement and an expression repeatedly until the value of the expression is false.
    | Do (XStm x) (XSP x) (Stmt x) (Exp x)
    -- | A @break@ statement transfers control out of an enclosing statement.
    | Break (XStm x) (XSP x) (Maybe (Ident x))
    -- | A @continue@ statement may occur only in a while, do, or for statement. Control passes to the loop-continuation
    --   point of that statement.
    | Continue (XStm x) (XSP x) (Maybe (Ident x))
    -- A @return@ statement returns control to the invoker of a method or constructor.
    | Return (XStm x) (XSP x) (Maybe (Exp x))
    -- | A @synchronized@ statement acquires a mutual-exclusion lock on behalf of the executing thread, executes a block,
    --   then releases the lock. While the executing thread owns the lock, no other thread may acquire the lock.
    | Synchronized (XStm x) (XSP x) (Exp x) (Block x)
    -- | A @throw@ statement causes an exception to be thrown.
    | Throw (XStm x) (XSP x) (Exp x)
    -- | A try statement executes a block. If a value is thrown and the try statement has one or more catch clauses that
    --   can catch it, then control will be transferred to the first such catch clause. If the try statement has a finally
    --   clause, then another block of code is executed, no matter whether the try block completes normally or abruptly,
    --   and no matter whether a catch clause is first given control.
    | Try (XStm x) (XSP x) (Block x) [Catch x] (Maybe {- finally -} (Block x))
    -- | Statements may have label prefixes.
    | Labeled (XStm x) (XSP x) (Ident x) (Stmt x)

-- Paragon
    -- | Locks can be opened or closed.
    | Open  (XStm x) (XSP x) (Lock x)
    | Close (XStm x) (XSP x) (Lock x)
    | OpenBlock  (XStm x) (XSP x) (Lock x) (Block x)
    | CloseBlock (XStm x) (XSP x) (Lock x) (Block x)
{-    -- A @when@ statement is a variant of @if@ that only tests whether locks are open.
    | WhenThen     Lock Stmt
    | WhenThenElse Lock Stmt Stmt    -}
  deriving (Eq,Ord,Show,Typeable,Data,Functor)
type family XStm x

-- | If a value is thrown and the try statement has one or more catch clauses that can catch it, then control will be
--   transferred to the first such catch clause.
data Catch x = Catch (XCatch x) (XSP x) (FormalParam x) (Block x)
  deriving (Eq,Ord,Show,Typeable,Data,Functor)
type family XCatch x

-- | A block of code labelled with a @case@ or @default@ within a @switch@ statement.
data SwitchBlock x
    = SwitchBlock (XSwitchBlock x) (XSP x) (SwitchLabel x) [BlockStmt x]
  deriving (Eq,Ord,Show,Typeable,Data,Functor)
type family XSwitchBlock x

-- | A label within a @switch@ statement.
data SwitchLabel x
    -- | The expression contained in the @case@ must be a 'Lit' or an @enum@ constant.
    = SwitchCase (XSwitchLabel x) (XSP x) (Exp x)
    | Default (XSwitchLabel x) (XSP x)
  deriving (Eq,Ord,Show,Typeable,Data,Functor)
type family XSwitchLabel x

-- | Initialization code for a basic @for@ statement.
data ForInit x
    = ForLocalVars (XForInit x) (XSP x) [Modifier x] (Type x) [VarDecl x]
    | ForInitExps (XForInit x) (XSP x) [Exp x]
  deriving (Eq,Ord,Show,Typeable,Data,Functor)
type family XForInit x

-- | An exception type has to be a class type or a type variable.
type ExceptionType x = RefType x -- restricted to ClassType or TypeVariable

data ExceptionSpec x = ExceptionSpec (XExceptionSpec x) (XSP x) [Modifier x] (ExceptionType x)
  deriving (Eq,Ord,Show,Typeable,Data,Functor)
type family XExceptionSpec x


-----------------------------------------------------------------------
-- Expressions

-- | Arguments to methods and constructors are expressions.
type Argument x = (Exp x)

-- | A Java expression.
data Exp x
    -- | A literal denotes a fixed, unchanging value.
    = Lit (XExp x) (XSP x) (Literal x)
    -- | A class literal, which is an expression consisting of the name of a class, interface, array,
    --   or primitive type, or the pseudo-type void (modelled by 'Nothing'), followed by a `.' and the token class.
    | ClassLit (XExp x) (XSP x) (Maybe (Type x))
    -- | The keyword @this@ denotes a value that is a reference to the object for which the instance method
    --   was invoked, or to the object being constructed.
    | This (XExp x) (XSP x)
    -- | Any lexically enclosing instance can be referred to by explicitly qualifying the keyword this.
    | ThisClass (XExp x) (XSP x) (Name x)
    -- | A parenthesized expression is a primary expression whose type is the type of the contained expression
    --   and whose value at run time is the value of the contained expression. If the contained expression
    --   denotes a variable then the parenthesized expression also denotes that variable.
    | Paren (XExp x) (XSP x) (Exp x)
    -- | A class instance creation expression is used to create new objects that are instances of classes.
    -- | The first argument is a list of non-wildcard type arguments to a generic constructor.
    --   What follows is the type to be instantiated, the list of arguments passed to the constructor, and
    --   optionally a class body that makes the constructor result in an object of an /anonymous/ class.
    | InstanceCreation (XExp x) (XSP x) [TypeArgument x] (ClassType x) [Argument x] (Maybe (ClassBody x))
    -- | A qualified class instance creation expression enables the creation of instances of inner member classes
    --   and their anonymous subclasses.
    | QualInstanceCreation (XExp x) (XSP x) (Exp x) [TypeArgument x] (Ident x) [Argument x] (Maybe (ClassBody x))
    -- | An array instance creation expression is used to create new arrays. The last argument denotes the number
    --   of dimensions that have no explicit length given. These dimensions must be given last.
    | ArrayCreate (XExp x) (XSP x) (Type x) [(Exp x, Maybe (Policy x))] [Maybe (Policy x)]
    -- | An array instance creation expression may come with an explicit initializer. Such expressions may not
    --   be given explicit lengths for any of its dimensions.
    | ArrayCreateInit (XExp x) (XSP x) (Type x) [Maybe (Policy x)] (ArrayInit x)
    -- | A field access expression.
    | FieldAccess (XExp x) (XSP x) (FieldAccess x)
    -- | A method invocation expression.
    | MethodInv (XExp x) (XSP x) (MethodInvocation x)
    -- | An array access expression refers to a variable that is a component of an array.
    | ArrayAccess (XExp x) (XSP x) (ArrayIndex x)
    -- | An expression name, e.g. a variable.
    | ExpName (XExp x) (XSP x) (Name x)
    -- | Post-incrementation expression, i.e. an expression followed by @++@.
    | PostIncrement (XExp x) (XSP x) (Exp x)
    -- | Post-decrementation expression, i.e. an expression followed by @--@.
    | PostDecrement (XExp x) (XSP x) (Exp x)
    -- | Pre-incrementation expression, i.e. an expression preceded by @++@.
    | PreIncrement  (XExp x) (XSP x) (Exp x)
    -- | Pre-decrementation expression, i.e. an expression preceded by @--@.
    | PreDecrement  (XExp x) (XSP x) (Exp x)
    -- | Unary plus, the promotion of the value of the expression to a primitive numeric type.
    | PrePlus  (XExp x) (XSP x) (Exp x)
    -- | Unary minus, the promotion of the negation of the value of the expression to a primitive numeric type.
    | PreMinus (XExp x) (XSP x) (Exp x)
    -- | Unary bitwise complementation: note that, in all cases, @~x@ equals @(-x)-1@.
    | PreBitCompl (XExp x) (XSP x) (Exp x)
    -- | Logical complementation of boolean values.
    | PreNot  (XExp x) (XSP x) (Exp x)
    -- | A cast expression converts, at run time, a value of one numeric type to a similar value of another
    --   numeric type; or confirms, at compile time, that the type of an expression is boolean; or checks,
    --   at run time, that a reference value refers to an object whose class is compatible with a specified
    --   reference type.
    | Cast (XExp x) (XSP x) (Type x) (Exp x)
    -- | The application of a binary operator to two operand expressions.
    | BinOp (XExp x) (XSP x) (Exp x) (Op x) (Exp x)
    -- | Testing whether the result of an expression is an instance of some reference type.
    | InstanceOf (XExp x) (XSP x) (Exp x) (RefType x)
    -- | The conditional operator @? :@ uses the boolean value of one expression to decide which of two other
    --   expressions should be evaluated.
    | Cond (XExp x) (XSP x) (Exp x) (Exp x) (Exp x)
    -- | Assignment of the result of an expression to a variable.
    | Assign (XExp x) (XSP x) (Lhs x) (AssignOp x) (Exp x)

-- Paragon
    | PolicyExp (XExp x) (XSP x) (PolicyExp x)
--    | PolicyOf (Ident x)
    | LockExp (XExp x) (XSP x) (Lock x)

-- Quasi-quotation
    | AntiQExp (XExp x) (XSP x) String

  deriving (Eq,Ord,Show,Typeable,Data,Functor)
type family XExp x
-- | A literal denotes a fixed, unchanging value.
data Literal  x
    = Int     (XLiteral x) (XSP x) Integer
    | Word    (XLiteral x) (XSP x) Integer
    | Float   (XLiteral x) (XSP x) Double
    | Double  (XLiteral x) (XSP x) Double
    | Boolean (XLiteral x) (XSP x) Bool
    | Char    (XLiteral x) (XSP x) Char
    | String  (XLiteral x) (XSP x) String
    | Null    (XLiteral x) (XSP x)
  deriving (Eq,Ord,Show,Typeable,Data,Functor)
type family XLiteral x
-- | A binary infix operator.
data Op x
    = Mult   (XOp x) (XSP x) | Div     (XOp x) (XSP x) | Rem    (XOp x) (XSP x)
    | Add    (XOp x) (XSP x) | Sub     (XOp x) (XSP x) | LShift (XOp x) (XSP x)
    | RShift (XOp x) (XSP x) | RRShift (XOp x) (XSP x) | LThan  (XOp x) (XSP x)
    | GThan  (XOp x) (XSP x) | LThanE  (XOp x) (XSP x) | GThanE (XOp x) (XSP x)
    | Equal  (XOp x) (XSP x) | NotEq   (XOp x) (XSP x) | And    (XOp x) (XSP x)
    | Or     (XOp x) (XSP x) | Xor     (XOp x) (XSP x) | CAnd   (XOp x) (XSP x)
    | COr    (XOp x) (XSP x) 
  deriving   (Eq,Ord,Show,Typeable,Data,Functor)
type family XOp x

-- | An assignment operator.
-- type families: XAssignOp
data AssignOp x
    = EqualA  (XAssignOp x) (XSP x)
    | MultA   (XAssignOp x) (XSP x) | DivA     (XAssignOp x) (XSP x)
    | RemA    (XAssignOp x) (XSP x) | AddA     (XAssignOp x) (XSP x)
    | SubA    (XAssignOp x) (XSP x) | LShiftA  (XAssignOp x) (XSP x)
    | RShiftA (XAssignOp x) (XSP x) | RRShiftA (XAssignOp x) (XSP x)
    | AndA    (XAssignOp x) (XSP x) | XorA     (XAssignOp x) (XSP x)
    | OrA     (XAssignOp x) (XSP x)
  deriving    (Eq,Ord,Show,Typeable,Data,Functor)
type family XAssignOp x

-- | The left-hand side of an assignment expression. This operand may be a named variable, such as a local
--   variable or a field of the current object or class, or it may be a computed variable, as can result from
--   a field access or an array access.
data Lhs x
    = NameLhs (XLhs x) (XSP x)  (Name x)          -- ^ Assign to a variable
    | FieldLhs (XLhs x) (XSP x)  (FieldAccess x)  -- ^ Assign through a field access
    | ArrayLhs (XLhs x) (XSP x)  (ArrayIndex  x)  -- ^ Assign to an array
  deriving (Eq,Ord,Show,Typeable,Data,Functor)
type family XLhs x
-- | Array access
-- type families: XArrayIndex
data ArrayIndex x = ArrayIndex (XArrayIndex x) (XSP x) (Exp x) (Exp x)    -- ^ Index into an array
  deriving (Eq,Ord,Show,Typeable,Data,Functor)
type family XArrayIndex x

-- | A field access expression may access a field of an object or array, a reference to which is the value
--   of either an expression or the special keyword super.
data FieldAccess x
    = PrimaryFieldAccess (XFieldAccess x) (XSP x) (Exp x) (Ident x)     -- ^ Accessing a field of an object or array computed from an expression.
    | SuperFieldAccess   (XFieldAccess x) (XSP x) (Ident x)             -- ^ Accessing a field of the superclass.
    | ClassFieldAccess   (XFieldAccess x) (XSP x) (Name x) (Ident x)    -- ^ Accessing a (static) field of a named class.
  deriving (Eq,Ord,Show,Typeable,Data,Functor)
type family XFieldAccess x

-- | A method invocation expression is used to invoke a class or instance method.
-- type families: XMethodInvocation
data MethodInvocation x
    -- | Invoking (XMethodInvocation x) (XSP x) specific named method.
    = MethodCallOrLockQuery (XMethodInvocation x) (XSP x) (Name x) [Argument x]
    -- | Invoking (XMethodInvocation x) (XSP x) method of (XMethodInvocation x) (XSP x) class computed from (XMethodInvocation x) (XSP x) primary expression, giving arguments for any generic type parameters.
    | PrimaryMethodCall (XMethodInvocation x) (XSP x) (Exp x) [NonWildTypeArgument x] (Ident x) [Argument x]
    -- | Invoking (XMethodInvocation x) (XSP x) method of the super class, giving arguments for any generic type parameters.
    | SuperMethodCall (XMethodInvocation x) (XSP x) [NonWildTypeArgument x] (Ident x) [Argument x]
    -- | Invoking (XMethodInvocation x) (XSP x) method of the superclass of (XMethodInvocation x) (XSP x) named class, giving arguments for any generic type parameters.
    | ClassMethodCall (XMethodInvocation x) (XSP x) (Name x) [NonWildTypeArgument x] (Ident x) [Argument x]
    -- | Invoking (XMethodInvocation x) (XSP x) method of (XMethodInvocation x) (XSP x) named type, giving arguments for any generic type parameters.
    | TypeMethodCall (XMethodInvocation x) (XSP x) (Name x) [NonWildTypeArgument x] (Ident x) [Argument x]
  deriving (Eq,Ord,Show,Typeable,Data,Functor)
type family XMethodInvocation x
-- | An array initializer may be specified in a declaration, or as part of an array creation expression, creating an
--   array and providing some initial values
-- type families: XArrayInit
data ArrayInit x
    = ArrayInit (XArrayInit x) [VarInit x]
  deriving (Eq,Ord,Show,Typeable,Data,Functor)
type family XArrayInit x

-----------------------------------------------------------------------
-- Types

data ReturnType x
    = VoidType (XReturnType x) (XSP x)
    | LockType (XReturnType x) (XSP x)
    | Type (XReturnType x) (XSP x) (Type x)
  deriving (Eq,Ord,Show,Typeable,Data,Functor)
type family XReturnType x

-- | There are two kinds of types in the Java programming language: primitive types and reference types.
-- type families: XType
data Type x
    = PrimType (XType x) (XSP x) (PrimType x)
    | RefType (XType x) (XSP x) (RefType x)
    | AntiQType (XType x) (XSP x) String
  deriving (Eq,Ord,Show,Typeable,Data,Functor)
type family XType x

-- | There are three kinds of reference types: class types, interface types, and array types.
--   Reference types may be parameterized with type arguments.
--   Type variables are introduced by generic type parameters.
-- type families: XRefType 
data RefType x
    = ClassRefType (XRefType x) (XSP x) (ClassType x)
    | TypeVariable (XRefType x) (XSP x) (Ident x)
    | ArrayType    (XRefType x) (XSP x) (Type x) [Maybe (Policy x)]
    -- ^ The second argument to ArrayType is the base type, and should not be an array type
  deriving (Eq,Ord,Show,Typeable,Data,Functor)
type family XRefType x

-- | A class or interface type consists of a type declaration specifier,
--   optionally followed by type arguments (in which case it is a parameterized type).
-- type families: XClassType
data ClassType x
    = ClassType(XClassType x) (XSP x) (Name x) [TypeArgument x]
  deriving (Eq,Ord,Show,Typeable,Data,Functor)
type family XClassType x

-- | Type arguments may be either reference types or wildcards.
-- type families: XTypeArgument
data TypeArgument x
    = Wildcard  (XTypeArgument x) (XSP x) (Maybe (WildcardBound x))
    | ActualArg (XTypeArgument x) (XSP x) (NonWildTypeArgument x)
  deriving (Eq,Ord,Show,Typeable,Data,Functor)
type family XTypeArgument x

data NonWildTypeArgument x
    = ActualName (XNonWildTypeArgument x) (XSP x) (Name x)      -- Can mean (XNonWildTypeArgument x) (XSP x) type or an exp
    | ActualType (XNonWildTypeArgument x) (XSP x) (RefType x)
    | ActualExp (XNonWildTypeArgument x) (XSP x) (Exp x)        -- Constrained to argExp
    | ActualLockState (XNonWildTypeArgument x) (XSP x) [Lock x]
  deriving (Eq,Ord,Show,Typeable,Data,Functor)
type family XNonWildTypeArgument x


-- | Wildcards may be given explicit bounds, either upper (@extends@) or lower (@super@) bounds.
-- type families: XWildcardBound
data WildcardBound x
    = ExtendsBound (XWildcardBound x) (XSP x) (RefType x)
    | SuperBound (XWildcardBound x) (XSP x) (RefType x)
  deriving (Eq,Ord,Show,Typeable,Data,Functor)
type family XWildcardBound x

-- | A primitive type is predefined by the Java programming language and named by its reserved keyword.
-- type families: XPrimType
data PrimType  x
    = BooleanT(XPrimType x) (XSP x)
    | ByteT   (XPrimType x) (XSP x)
    | ShortT  (XPrimType x) (XSP x)
    | IntT    (XPrimType x) (XSP x)
    | LongT   (XPrimType x) (XSP x)
    | CharT   (XPrimType x) (XSP x)
    | FloatT  (XPrimType x) (XSP x)
    | DoubleT (XPrimType x) (XSP x)
-- Paragon
    | ActorT  (XPrimType x) (XSP x)
    | PolicyT (XPrimType x) (XSP x)
  deriving (Eq,Ord,Show,Typeable,Data,Functor)
type family XPrimType x

aOfPrimType :: PrimType a -> a
aOfPrimType (BooleanT x) = x
aOfPrimType (ByteT x)    = x
aOfPrimType (ShortT x)   = x
aOfPrimType (IntT x)     = x
aOfPrimType (LongT x)    = x
aOfPrimType (CharT x)    = x
aOfPrimType (FloatT x)   = x
aOfPrimType (DoubleT x)  = x
aOfPrimType (ActorT x)   = x
aOfPrimType (PolicyT x)  = x

-- | A class is generic if it declares one or more type variables. These type variables are known
--   as the type parameters of the class.
--   Paragon adds three new forms - actor, policy and lockstate parameters.
-- type families: XTypeParam
data TypeParam x = TypeParam (XTypeParam x) (XSP x) (Ident x) [RefType x]
-- Paragon
                 | ActorParam    (XTypeParam x) (XSP x) (RefType x) (Ident x)
                 | PolicyParam   (XTypeParam x) (XSP x) (Ident x)
                 | LockStateParam(XTypeParam x) (XSP x) (Ident x)
  deriving (Eq,Ord,Show,Typeable,Data,Functor)
type family XTypeParam x

-----------------------------------------------------------------------
-- Paragon

type Policy a = Exp a

-- | A policy is a conjunction (set) of clauses, represented as a list.
--data PolicyLit = PolicyLit [Clause Actor]
-- type families : XPolicyExp
data PolicyExp x = PolicyLit(XPolicyExp x) (XSP x)[Clause x]
                 | PolicyOf (XPolicyExp x) (XSP x)(Ident x)
                 | PolicyThis(XPolicyExp x) (XSP x)
                 | PolicyTypeVar(XPolicyExp x) (XSP x) (Ident x)
  deriving (Eq,Ord,Show,Typeable,Data,Functor)
type family XPolicyExp x


-- | A lock property is a potentially recursive policy with an atom head.
-- type families: XLockProperties
data LockProperties x = LockProperties (XLockProperties x) (XSP x) [LClause x]
  deriving (Eq,Ord,Show,Typeable,Data,Functor)
type family XLockProperties x

-- HERE
-- | A clause of the form Sigma => a, where a is an actor and Sigma a set of
--   locks/atomic predicates that must be open/true.
-- type families: XClause
data Clause x = Clause (XClause x) (XSP x) [ClauseVarDecl x] (ClauseHead x) [Atom x]
  deriving (Eq,Ord,Show,Typeable,Data,Functor)
type family XClause x

-- | type families : XClauseVarDecl
data ClauseVarDecl x = ClauseVarDecl (XClauseVarDecl x) (XSP x) (RefType x) (Ident x)
  deriving (Eq,Ord,Show,Typeable,Data,Functor)
type family XClauseVarDecl x

-- | type families : XClauseHead
data ClauseHead x = ClauseDeclHead (XClauseHead x) (XSP x) (ClauseVarDecl x)
                  | ClauseVarHead (XClauseHead x) (XSP x) (Actor x)
  deriving (Eq,Ord,Show,Typeable,Data,Functor)
type family XClauseHead x

-- | type families: XLClause
data LClause x = LClause (XLClause x) (XSP x) [ClauseVarDecl x] (Atom x) [Atom x]
               | ConstraintClause (XLClause x) (XSP x) [ClauseVarDecl x] [Atom x]
  deriving (Eq,Ord,Show,Typeable,Data,Functor)
type family XLClause x

-- | An actor variable, either forall-quantified within the current clause, or
--   free and thus concrete w.r.t. the policy under scrutiny.
-- type families: XActor
data Actor x = Actor (XActor x) (XSP x) (ActorName x)    -- ^ Free actor variables
             | Var   (XActor x) (XSP x) (Ident x)        -- ^ Quantified actor variables
  deriving (Eq,Ord,Show,Typeable,Data,Functor)
type family XActor x
-- | Type families: XActorName
data ActorName x
    = ActorName (XActorName x) (XSP x) (Name x)
    -- ^ A free actor variable
    | ActorTypeVar (XActorName x) (XSP x) (RefType x) (Ident x)
    -- ^ A free actor type parameter
  deriving (Eq,Ord,Show,Typeable,Data,Functor)
type family XActorName x

-- | A lock is an atomic n-ary predicate.
-- Type families: XAtom
data Atom x = Atom (XAtom x) (XSP x) (Name x) [Actor x]
  deriving (Eq,Ord,Show,Typeable,Data,Functor)
type family XAtom x

-- | Type families: XLock
data Lock x = Lock (XLock x) (XSP x) (Name x) [ActorName x] | LockVar (XLock x) (XSP x) (Ident x)
  deriving (Eq,Ord,Show,Typeable,Data,Functor)
type family XLock x

-----------------------------------------------------------------------
-- Useful accessors

importDeclName :: ImportDecl a -> Name a
importDeclName (SingleTypeImport     _ n)   = n
importDeclName (TypeImportOnDemand   _ n)   = n
importDeclName (SingleStaticImport   _ n _) = n
importDeclName (StaticImportOnDemand _ n)   = n


-----------------------------------------------------------------------
-- Names and identifiers

-- | A single identifier.
-- Type families: XIdent
data Ident x = Ident (XIdent x) (XSP x) B.ByteString | AntiQIdent (XIdent x) (XSP x) String
  deriving (Eq,Ord,Show,Typeable,Data,Functor)
type family XIdent x

-- | Extract actual identifier string from Ident wrapper type
unIdent :: Ident a -> B.ByteString
unIdent (Ident _ bs) = bs
unIdent (AntiQIdent _ str) = panic (syntaxModule ++ ".unIdent")
            $ "AntiQIdent " ++ str

-- | A name, i.e. a period-separated list of identifiers.
-- type families : XName
data Name x = Name (XName x) NameType (Maybe (Name x)) (Ident x)
            | AntiQName (XName x) (XSP x) String
   -- Show removed to get more readable debug output
type family XName x

deriving instance Typeable (XName x) => Typeable (Name x)
deriving instance Eq       (XName x) => Eq       (Name x)
deriving instance Ord      (XName x) => Ord      (Name x)
deriving instance Data     (XName x) => Data     (Name x)
-- Prints name as a simple string to be easier to read.
-- To get printout of the whole recursive name structure, comment this out and put
-- Show in the deriving clause.
instance Show (Name a) where
  show (Name _ _ nextBase (Ident _ iBase)) =
    show (showInner nextBase ++ B.unpack iBase)
    where
      showInner Nothing = ""
      showInner (Just (Name _ _ next (Ident _ i))) =  showInner next ++ B.unpack i ++ "."


data NameType
    = EName    -- ^Expression name
    | MName    -- ^Method name
    | TName    -- ^Type (class, interface, enum [not implemented]) name
    | PName    -- ^Package name
    | LName    -- ^Lock name
    | POrTName -- ^Either package or Type name
    | MOrLName -- ^Method or lock name
    | EOrLName -- ^Expression or lock name
    | AmbName  -- ^Ambiguous name
  deriving (Eq,Ord,Show,Typeable,Data)

nameType :: Name a -> NameType
nameType (Name _ nt _ _) = nt
nameType _ = panic (syntaxModule ++ ".nameType")
                   "AntiQName"

setNameType :: NameType -> Name a -> Name a
setNameType nt (Name a _ mPre i) = Name a nt mPre i
setNameType _ n = n

mkSimpleName :: NameType -> Ident a -> Name a
mkSimpleName nt i = Name (ann i) nt Nothing i

mkUniformName :: (a -> a -> a) -- Merge annotations
              -> NameType -> [Ident a] -> Name a
mkUniformName f nt ids = mkName' (reverse ids)
    where mkName' [] = panic (syntaxModule ++ ".mkUniformName")
                             "Empty list of idents"
          mkName' [i] = Name (ann i) nt Nothing i
          mkName' (i:is) =
              let pre = mkName' is
                  a = f (ann pre) (ann i)
              in Name a nt (Just pre) i

mkUniformName_ :: NameType -> [Ident a] -> Name a
mkUniformName_ = mkUniformName const

mkName :: (a -> a -> a) -- Merge annotations
       -> NameType -> NameType -> [Ident a] -> Name a
mkName f nt ntPre ids = mkName' (reverse ids)
    where mkName' [] = panic (syntaxModule ++ ".mkName")
                             "Empty list of idents"
          mkName' [i] = Name (ann i) nt Nothing i
          mkName' (i:is) =
              let pre = mkUniformName f ntPre (reverse is)
                  a = f (ann pre) (ann i)
              in Name a nt (Just pre) i

mkName_ :: NameType -> NameType -> [Ident a] -> Name a
mkName_ = mkName const

flattenName :: Name a -> [Ident a]
flattenName n = reverse $ flName n
    where flName (Name _ _ mPre i) = i : maybe [] flName mPre

          flName AntiQName{} = panic (syntaxModule ++ ".flattenName")
                                     "Cannot flatten name anti-quote"

mkIdent :: a -> String -> Ident a
mkIdent a = Ident a . B.pack

mkIdent_ :: String -> Ident SourcePos
mkIdent_ = mkIdent defaultPos

-----------------------------------------------------------------------
-- Annotations

$(deriveAnnMany
  [''CompilationUnit, ''PackageDecl, ''ImportDecl,
   ''TypeDecl, ''ClassDecl, ''ClassBody, ''EnumBody, ''EnumConstant,
   ''InterfaceDecl, ''InterfaceBody, ''Decl, ''MemberDecl,
   ''VarDecl, ''VarDeclId, ''VarInit, ''ArrayInit,
   ''FormalParam, ''MethodBody, ''ConstructorBody, ''ExplConstrInv,
   ''Modifier, ''Block, ''BlockStmt, ''Stmt,
   ''Catch, ''SwitchBlock, ''SwitchLabel, ''ForInit, ''ExceptionSpec,
   ''Exp, ''Literal, ''Op, ''AssignOp, ''Lhs,
   ''ArrayIndex, ''FieldAccess, ''MethodInvocation,
   ''Type, ''PrimType, ''RefType, ''ClassType, ''ReturnType,
   ''TypeArgument, ''NonWildTypeArgument, ''WildcardBound, ''TypeParam,
   ''PolicyExp, ''LockProperties, ''Clause, ''LClause,
   ''ClauseVarDecl, ''ClauseHead,
   ''Actor, ''ActorName, ''Atom, ''Lock, ''Ident, ''Name])
