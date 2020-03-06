{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE CPP, DeriveDataTypeable, DeriveFunctor, TemplateHaskell,
             FlexibleContexts, MultiParamTypeClasses, FlexibleInstances #-}

{-# LANGUAGE TypeFamilies, DataKinds, ConstraintKinds #-}
{-# LANGUAGE GADTs, EmptyCase, StandaloneDeriving #-}
{-# LANGUAGE TypeOperators, PatternSynonyms #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE DeriveAnyClass #-}
module Language.Java.Paragon.SyntaxTTG where
  -- (
  --   module Language.Java.Paragon.SyntaxTTG,
  --   module Language.Java.Paragon.Annotated
  --                                   )

import Data.Data


import Language.Java.Paragon.Interaction
import Language.Java.Paragon.SourcePos
import GHC.Types (Constraint)
import qualified Data.ByteString.Char8 as B


syntaxModule :: String
syntaxModule = libraryBase ++ ".Syntax"


-----------------------------------------------------------------------
-- Packages

-- Removed import Annotated atm, adding an error implementation
-- for ann

ann = error "Phasing out Annotatated"

-- | A compilation unit is the top level syntactic goal symbol of a Java program
-- This usually corresponds to a single .java source file that may start with
-- a package declaration, followed by a (possibly empty) list of imports and
-- a (usually non-empty) list of type declaration, where a type is a class
-- or interface.
-- Note that the paragon compiler currently only accepts a single type per
-- compilation unit and does not yet suport enums, although files containing
-- enums can be parsed
data CompilationUnit x = CompilationUnit (XCompilationUnit x) (Maybe (PackageDecl x)) [ImportDecl x] [TypeDecl x]
type family XCompilationUnit x

-- | A package declaration appears within a compilation unit to indicate the package to which the compilation unit belongs.
data PackageDecl x = PackageDecl (XPackageDecl x) (Name x)
type family XPackageDecl x

-------------------------------------------------------------------------------
-- #ifdef __GLASGOW_HASKELL__                                                --
-- type AllXPackageDecl (f :: * -> Constraint) x = (f (XPackageDecl x))      --
-- deriving instance AllXPackageDecl Typeable  x => Typeable (PackageDecl x) --
-- deriving instance AllXPackageDecl Eq        x => Eq       (PackageDecl x) --
-- deriving instance AllXPackageDecl Ord       x => Ord      (PackageDecl x) --
-- deriving instance AllXPackageDecl Data      x => Data     (PackageDecl x) --
-- #else                                                                     --
-- deriving instance AllXPackageDecl Eq        x => Eq       (PackageDecl x) --
-- deriving instance AllXPackageDecl Ord       x => Ord      (PackageDecl x) --
--                                                                           --
-- #endif                                                                    --
-------------------------------------------------------------------------------

-- | An import declaration allows a static member or a named type to be referred
-- to by a single unqualified identifier.
-- The first argument signals whether the declaration only imports static
-- members.
-- The last argument signals whether the declaration brings all names in the
-- named type or package, or only brings a single name into scope.
data ImportDecl x
    = SingleTypeImport     (XImportDecl x) (Name x)
      -- ^Import a single type (class/interface/enum)
    | TypeImportOnDemand   (XImportDecl x) (Name x)
      -- ^Bring all types of package into scope, e.g. import java.lang.util.*
    | SingleStaticImport   (XImportDecl x) (Name x) (Ident x)
      -- ^Single static import, e.g. import static java.lang.Math.PI
    | StaticImportOnDemand (XImportDecl x) (Name x)
      -- ^Static import of all members, e.g. import static java.lang.Math.*
type family XImportDecl x

-----------------------------------------------------------------------
-- Declarations

-- | A type declaration declares a class type or an interface type.
data TypeDecl x
    = ClassTypeDecl (XTypeDecl x) (ClassDecl x)
    | InterfaceTypeDecl (XTypeDecl x) (InterfaceDecl x)
type family XTypeDecl x

-- | A class declaration specifies a new named reference type.
-- Note that the compiler does not actually deal with enums yet!
data ClassDecl x
    = ClassDecl (XClassDecl x) [Modifier x] (Ident x) [TypeParam x] (Maybe (ClassType x)) [ClassType x] (ClassBody x)
      -- ^Fields: Class modifiers, class identifier, type params, super class,
      -- if any, list of implemented interfaces, class body
    | EnumDecl  (XClassDecl x) [Modifier x] (Ident x)                                     [ClassType x] (EnumBody x)
type family XClassDecl x

-- | A class body may contain declarations of members of the class, that is,
--   fields, classes, interfaces and methods.
--   A class body may also contain instance initializers, static
--   initializers, and declarations of constructors for the class.
data ClassBody x = ClassBody (XClassBody x) [Decl x]
type family XClassBody x

-- | The body of an enum type may contain enum constants.
data EnumBody x = EnumBody (XEnumBody x) [EnumConstant x] [Decl x]
type family XEnumBody x

-- | An enum constant defines an instance of the enum type.
data EnumConstant x = EnumConstant (XEnumConstant x) (Ident x) [Argument x] (Maybe (ClassBody x))
type family XEnumConstant x

-- | An interface declaration introduces a new reference type whose members
--   are classes, interfaces, constants and abstract methods. This type has
--   no implementation, but otherwise unrelated classes can implement it by
--   providing implementations for its abstract methods.
data InterfaceDecl x
    = InterfaceDecl (XInterfaceDecl x) [Modifier x] (Ident x) [TypeParam x] [ClassType x] (InterfaceBody x)
type family XInterfaceDecl x

-- | The body of an interface may declare members of the interface.
data InterfaceBody x
    = InterfaceBody (XInterfaceBody x) [MemberDecl x]
type family XInterfaceBody x

-- | A declaration is either a member declaration, or a declaration of an
--   initializer, which may be static.
data Decl x
    = MemberDecl (XDecl x) (MemberDecl x)
    | InitDecl (XDecl x) Bool (Block x)
type family XDecl x


-- | A class or interface member can be an inner class or interface, a field or
--   constant, or a method or constructor. An interface may only have as members
--   constants (not fields), abstract methods, and no constructors.
data MemberDecl x
    -- | The variables of a class type are introduced by field declarations.
    = FieldDecl (XMemberDecl x) [Modifier x] (Type x) [VarDecl x]
    -- | A method declares executable code that can be invoked, passing a fixed number of values as arguments.
    | MethodDecl (XMemberDecl x) [Modifier x] [TypeParam x] (ReturnType x) (Ident x) [FormalParam x] [ExceptionSpec x] (MethodBody x)
    -- | A constructor is used in the creation of an object that is an instance of a class.
    | ConstructorDecl (XMemberDecl x) [Modifier x] [TypeParam x]                  (Ident x) [FormalParam x] [ExceptionSpec x] (ConstructorBody x)
    -- | A member class is a class whose declaration is directly enclosed in another class or interface declaration.
    | MemberClassDecl (XMemberDecl x) (ClassDecl x)
    -- | A member interface is an interface whose declaration is directly enclosed in another class or interface declaration.
    | MemberInterfaceDecl (XMemberDecl x) (InterfaceDecl x)

-- Paragon
    -- | A lock declaration is a special kind of field declaration.
    | LockDecl (XMemberDecl x) [Modifier x]  (Ident x) [RefType x] (Maybe (LockProperties x))
{-    -- | A policy declaration - should be a field decl really.
    | PolicyDecl a [Modifier a] Ident Policy -}
{-    -- | An actor declaration is a special kind of field declaration.
    | ActorDecl [Modifier] Ident (Maybe VarInit) -}
type family XMemberDecl x

-- int x; => VarDecl (VarId "x") Nothing
-- int x = 1; => VarDecl (VarId "x") (Just ...)
-- | A declaration of a variable, which may be explicitly initialized.
data VarDecl x
    = VarDecl (XVarDecl x) (VarDeclId x) (Maybe (VarInit x))
type family XVarDecl x

-- | The name of a variable in a declaration, which may be an array.
data VarDeclId x
    = VarId (XVarDeclId x) (Ident x)
    | VarDeclArray (XVarDeclId x) (VarDeclId x)
    -- ^ Multi-dimensional arrays are represented by nested applications of 'VarDeclArray' (Deprecated)
type family XVarDeclId x

getVarDeclId :: VarDeclId a -> Ident a
getVarDeclId (VarId _ ident) = ident
getVarDeclId (VarDeclArray _ varDeclId) = getVarDeclId varDeclId

-- | Explicit initializer for a variable declaration.
data VarInit x
    = InitExp (XVarInit x) (Exp x)
    | InitArray (XVarInit x) (ArrayInit x)
type family XVarInit x

-- | A formal parameter in method declaration. The last parameter
--   for a given declaration may be marked as variable arity,
--   indicated by the boolean argument.
data FormalParam x = FormalParam (XFormalParam x) [Modifier x] (Type x) Bool (VarDeclId x)
type family XFormalParam x

getFormalParamId :: FormalParam a -> Ident a
getFormalParamId (FormalParam _ _ _ _ varDeclId) = getVarDeclId varDeclId

-- | A method body is either a block of code that implements the method or simply a
--   semicolon, indicating the lack of an implementation (modelled by 'Nothing').
data MethodBody x = MethodBody (XMethodBody x) (Maybe (Block x))
type family XMethodBody x

-- | The first statement of a constructor body may be an explicit invocation of
--   another constructor of the same class or of the direct superclass.
data ConstructorBody x = ConstructorBody (XConstructorBody x) (Maybe (ExplConstrInv x)) [BlockStmt x]
type family XConstructorBody x

-- | An explicit constructor invocation invokes another constructor of the
--   same class, or a constructor of the direct superclass, which may
--   be qualified to explicitly specify the newly created object's immediately
--   enclosing instance.
data ExplConstrInv x
    = ThisInvoke         (XExplConstrInv x)         [NonWildTypeArgument x] [Argument x]
    | SuperInvoke        (XExplConstrInv x)         [NonWildTypeArgument x] [Argument x]
    | PrimarySuperInvoke (XExplConstrInv x) (Exp x) [NonWildTypeArgument x] [Argument x]
type family XExplConstrInv x

-- | A modifier specifying properties of a given declaration. In general only
--   a few of these modifiers are allowed for each declaration type, for instance
--   a member type declaration may only specify one of public, private or protected.
data Modifier x
    = Public    (XMod x)
    | Private   (XMod x)
    | Protected (XMod x)
    | Abstract  (XMod x)
    | Final     (XMod x)
    | Static    (XMod x)
    | StrictFP  (XMod x)
    | Transient (XMod x)
    | Volatile  (XMod x)
    | Native    (XMod x)

    | Typemethod (XMod x)
    | Reflexive  (XMod x)
    | Transitive (XMod x)
    | Symmetric  (XMod x)
    | Readonly   (XMod x)
    | Notnull    (XMod x)

    | Reads   (XMod x) (Policy x)
    | Writes  (XMod x) (Policy x)
    | Opens   (XMod x) [Lock x]
    | Closes  (XMod x) [Lock x]
    | Expects (XMod x) [Lock x]
type family XMod x

-- isMethodStatic :: (XMod -> a) -> [Modifier x] -> Bool
-- isMethodStatic f ms = Static () ()  `elem` removeAnnotationMany ms

-----------------------------------------------------------------------
-- Statements

-- | A block is a sequence of statements, local class declarations
--   and local variable declaration statements within braces.
data Block x = Block (XBlock x) [BlockStmt x]
type family XBlock x

-- | A block statement is either a normal statement, a local
--   class declaration or a local variable declaration.
data BlockStmt x
    = BlockStmt (XBlockStm x) (Stmt x)
    | LocalClass (XBlockStm x) (ClassDecl x)
    | LocalVars (XBlockStm x) [Modifier x] (Type x) [VarDecl x]

-- Paragon
    | LocalLock (XBlockStm x) [Modifier x] (Ident x) [RefType x] (Maybe (LockProperties x))
{-    | LocalPolicy [Modifier] Ident Policy
      | LocalActor [Modifier] Ident (Maybe VarInit) -}
type family XBlockStm x


-- | A Java statement.
data Stmt x
    -- | A statement can be a nested block.
    = StmtBlock (XStm x) (Block x)
    -- | The @if-then@ statement allows conditional execution of a statement.
    | IfThen (XStm x) (Exp x) (Stmt x)
    -- | The @if-then-else@ statement allows conditional choice of two statements, executing one or the other but not both.
    | IfThenElse (XStm x) (Exp x) (Stmt x) (Stmt x)
    -- | The @while@ statement executes an expression and a statement repeatedly until the value of the expression is false.
    | While (XStm x) (Exp x) (Stmt x)
    -- | The basic @for@ statement executes some initialization code, then executes an expression, a statement, and some
    --   update code repeatedly until the value of the expression is false.
    | BasicFor (XStm x) (Maybe (ForInit x)) (Maybe (Exp x)) (Maybe [Exp x]) (Stmt x)
    -- | The enhanced @for@ statement iterates over an array or a value of a class that implements the @iterator@ interface.
    | EnhancedFor (XStm x) [Modifier x] (Type x) (Ident x) (Exp x) (Stmt x)
    -- | An empty statement does nothing.
    | Empty (XStm x)
    -- | Certain kinds of expressions may be used as statements by following them with semicolons:
    --   assignments, pre- or post-inc- or decrementation, method invocation or class instance
    --   creation expressions.
    | ExpStmt (XStm x) (Exp x)
    -- | An assertion is a statement containing a boolean expression, where an error is reported if the expression
    --   evaluates to false.
    | Assert (XStm x) (Exp x) (Maybe (Exp x))
    -- | The switch statement transfers control to one of several statements depending on the value of an expression.
    | Switch (XStm x) (Exp x) [SwitchBlock x]
    -- | The @do@ statement executes a statement and an expression repeatedly until the value of the expression is false.
    | Do (XStm x) (Stmt x) (Exp x)
    -- | A @break@ statement transfers control out of an enclosing statement.
    | Break (XStm x) (Maybe (Ident x))
    -- | A @continue@ statement may occur only in a while, do, or for statement. Control passes to the loop-continuation
    --   point of that statement.
    | Continue (XStm x) (Maybe (Ident x))
    -- A @return@ statement returns control to the invoker of a method or constructor.
    | Return (XStm x) (Maybe (Exp x))
    -- | A @synchronized@ statement acquires a mutual-exclusion lock on behalf of the executing thread, executes a block,
    --   then releases the lock. While the executing thread owns the lock, no other thread may acquire the lock.
    | Synchronized (XStm x) (Exp x) (Block x)
    -- | A @throw@ statement causes an exception to be thrown.
    | Throw (XStm x) (Exp x)
    -- | A try statement executes a block. If a value is thrown and the try statement has one or more catch clauses that
    --   can catch it, then control will be transferred to the first such catch clause. If the try statement has a finally
    --   clause, then another block of code is executed, no matter whether the try block completes normally or abruptly,
    --   and no matter whether a catch clause is first given control.
    | Try (XStm x) (Block x) [Catch x] (Maybe {- finally -} (Block x))
    -- | Statements may have label prefixes.
    | Labeled (XStm x) (Ident x) (Stmt x)

-- Paragon
    -- | Locks can be opened or closed.
    | Open  (XStm x) (Lock x)
    | Close (XStm x) (Lock x)
    | OpenBlock  (XStm x) (Lock x) (Block x)
    | CloseBlock (XStm x) (Lock x) (Block x)
{-    -- A @when@ statement is a variant of @if@ that only tests whether locks are open.
    | WhenThen     Lock Stmt
    | WhenThenElse Lock Stmt Stmt    -}
type family XStm x

-- | If a value is thrown and the try statement has one or more catch clauses that can catch it, then control will be
--   transferred to the first such catch clause.
data Catch x = Catch (XCatch x) (FormalParam x) (Block x)
type family XCatch x

-- | A block of code labelled with a @case@ or @default@ within a @switch@ statement.
data SwitchBlock x
    = SwitchBlock (XSwitchBlock x) (SwitchLabel x) [BlockStmt x]
type family XSwitchBlock x

-- | A label within a @switch@ statement.
data SwitchLabel x
    -- | The expression contained in the @case@ must be a 'Lit' or an @enum@ constant.
    = SwitchCase (XSwitchLabel x) (Exp x)
    | Default (XSwitchLabel x)
type family XSwitchLabel x

-- | Initialization code for a basic @for@ statement.
data ForInit x
    = ForLocalVars (XForInit x) [Modifier x] (Type x) [VarDecl x]
    | ForInitExps (XForInit x) [Exp x]
type family XForInit x

-- | An exception type has to be a class type or a type variable.
type ExceptionType x = RefType x -- restricted to ClassType or TypeVariable

data ExceptionSpec x = ExceptionSpec (XExceptionSpec x) [Modifier x] (ExceptionType x)
type family XExceptionSpec x


-----------------------------------------------------------------------
-- Expressions

-- | Arguments to methods and constructors are expressions.
type Argument x = (Exp x)

-- | A Java expression.
data Exp x
    -- | A literal denotes a fixed, unchanging value.
    = Lit (XExp x) (Literal x)
    -- | A class literal, which is an expression consisting of the name of a class, interface, array,
    --   or primitive type, or the pseudo-type void (modelled by 'Nothing'), followed by a `.' and the token class.
    | ClassLit (XExp x) (Maybe (Type x))
    -- | The keyword @this@ denotes a value that is a reference to the object for which the instance method
    --   was invoked, or to the object being constructed.
    | This (XExp x)
    -- | Any lexically enclosing instance can be referred to by explicitly qualifying the keyword this.
    | ThisClass (XExp x) (Name x)
    -- | A parenthesized expression is a primary expression whose type is the type of the contained expression
    --   and whose value at run time is the value of the contained expression. If the contained expression
    --   denotes a variable then the parenthesized expression also denotes that variable.
    | Paren (XExp x) (Exp x)
    -- | A class instance creation expression is used to create new objects that are instances of classes.
    -- | The first argument is a list of non-wildcard type arguments to a generic constructor.
    --   What follows is the type to be instantiated, the list of arguments passed to the constructor, and
    --   optionally a class body that makes the constructor result in an object of an /anonymous/ class.
    | InstanceCreation (XExp x) [TypeArgument x] (ClassType x) [Argument x] (Maybe (ClassBody x))
    -- | A qualified class instance creation expression enables the creation of instances of inner member classes
    --   and their anonymous subclasses.
    | QualInstanceCreation (XExp x) (Exp x) [TypeArgument x] (Ident x) [Argument x] (Maybe (ClassBody x))
    -- | An array instance creation expression is used to create new arrays. The last argument denotes the number
    --   of dimensions that have no explicit length given. These dimensions must be given last.
    | ArrayCreate (XExp x) (Type x) [(Exp x, Maybe (Policy x))] [Maybe (Policy x)]
    -- | An array instance creation expression may come with an explicit initializer. Such expressions may not
    --   be given explicit lengths for any of its dimensions.
    | ArrayCreateInit (XExp x) (Type x) [Maybe (Policy x)] (ArrayInit x)
    -- | A field access expression.
    | FieldAccess (XExp x) (FieldAccess x)
    -- | A method invocation expression.
    | MethodInv (XExp x) (MethodInvocation x)
    -- | An array access expression refers to a variable that is a component of an array.
    | ArrayAccess (XExp x) (ArrayIndex x)
    -- | An expression name, e.g. a variable.
    | ExpName (XExp x) (Name x)
    -- | Post-incrementation expression, i.e. an expression followed by @++@.
    | PostIncrement (XExp x) (Exp x)
    -- | Post-decrementation expression, i.e. an expression followed by @--@.
    | PostDecrement (XExp x) (Exp x)
    -- | Pre-incrementation expression, i.e. an expression preceded by @++@.
    | PreIncrement  (XExp x) (Exp x)
    -- | Pre-decrementation expression, i.e. an expression preceded by @--@.
    | PreDecrement  (XExp x) (Exp x)
    -- | Unary plus, the promotion of the value of the expression to a primitive numeric type.
    | PrePlus  (XExp x) (Exp x)
    -- | Unary minus, the promotion of the negation of the value of the expression to a primitive numeric type.
    | PreMinus (XExp x) (Exp x)
    -- | Unary bitwise complementation: note that, in all cases, @~x@ equals @(-x)-1@.
    | PreBitCompl (XExp x) (Exp x)
    -- | Logical complementation of boolean values.
    | PreNot  (XExp x) (Exp x)
    -- | A cast expression converts, at run time, a value of one numeric type to a similar value of another
    --   numeric type; or confirms, at compile time, that the type of an expression is boolean; or checks,
    --   at run time, that a reference value refers to an object whose class is compatible with a specified
    --   reference type.
    | Cast (XExp x) (Type x) (Exp x)
    -- | The application of a binary operator to two operand expressions.
    | BinOp (XExp x) (Exp x) (Op x) (Exp x)
    -- | Testing whether the result of an expression is an instance of some reference type.
    | InstanceOf (XExp x) (Exp x) (RefType x)
    -- | The conditional operator @? :@ uses the boolean value of one expression to decide which of two other
    --   expressions should be evaluated.
    | Cond (XExp x) (Exp x) (Exp x) (Exp x)
    -- | Assignment of the result of an expression to a variable.
    | Assign (XExp x) (Lhs x) (AssignOp x) (Exp x)

-- Paragon
    | PolicyExp (XExp x) (PolicyExp x)
--    | PolicyOf (Ident x)
    | LockExp (XExp x) (Lock x)

-- Quasi-quotation
    | AntiQExp (XExp x) String
type family XExp x

-- | A literal denotes a fixed, unchanging value.
data Literal  x
    = Int     (XLiteral x) Integer
    | Word    (XLiteral x) Integer
    | Float   (XLiteral x) Double
    | Double  (XLiteral x) Double
    | Boolean (XLiteral x) Bool
    | Char    (XLiteral x) Char
    | String  (XLiteral x) String
    | Null    (XLiteral x)
type family XLiteral x

-- | A binary infix operator.
data Op x
    = Mult   (XOp x) | Div     (XOp x) | Rem    (XOp x)
    | Add    (XOp x) | Sub     (XOp x) | LShift (XOp x)
    | RShift (XOp x) | RRShift (XOp x) | LThan  (XOp x)
    | GThan  (XOp x) | LThanE  (XOp x) | GThanE (XOp x)
    | Equal  (XOp x) | NotEq   (XOp x) | And    (XOp x)
    | Or     (XOp x) | Xor     (XOp x) | CAnd   (XOp x)
    | COr    (XOp x)
type family XOp x

-- | An assignment operator.
-- type families: XAssignOp
data AssignOp x
    = EqualA  (XAssignOp x)
    | MultA   (XAssignOp x) | DivA     (XAssignOp x)
    | RemA    (XAssignOp x) | AddA     (XAssignOp x)
    | SubA    (XAssignOp x) | LShiftA  (XAssignOp x)
    | RShiftA (XAssignOp x) | RRShiftA (XAssignOp x)
    | AndA    (XAssignOp x) | XorA     (XAssignOp x)
    | OrA     (XAssignOp x)
type family XAssignOp x

-- | The left-hand side of an assignment expression. This operand may be a named variable, such as a local
--   variable or a field of the current object or class, or it may be a computed variable, as can result from
--   a field access or an array access.
data Lhs x
    = NameLhs (XLhs x)  (Name x)          -- ^ Assign to a variable
    | FieldLhs (XLhs x)  (FieldAccess x)  -- ^ Assign through a field access
    | ArrayLhs (XLhs x)  (ArrayIndex  x)  -- ^ Assign to an array
type family XLhs x

-- | Array access
-- type families: XArrayIndex
data ArrayIndex x = ArrayIndex (XArrayIndex x) (Exp x) (Exp x)    -- ^ Index into an array
type family XArrayIndex x

-- | A field access expression may access a field of an object or array, a reference to which is the value
--   of either an expression or the special keyword super.
data FieldAccess x
    = PrimaryFieldAccess (XFieldAccess x) (Exp x) (Ident x)     -- ^ Accessing a field of an object or array computed from an expression.
    | SuperFieldAccess   (XFieldAccess x) (Ident x)             -- ^ Accessing a field of the superclass.
    | ClassFieldAccess   (XFieldAccess x) (Name x) (Ident x)    -- ^ Accessing a (static) field of a named class.
type family XFieldAccess x

-- | A method invocation expression is used to invoke a class or instance method.
-- type families: XMethodInvocation
data MethodInvocation x
    -- | Invoking (XMethodInvocation x) specific named method.
    = MethodCallOrLockQuery (XMethodInvocation x) (Name x) [Argument x]
    -- | Invoking (XMethodInvocation x) method of (XMethodInvocation x) class computed from (XMethodInvocation x) primary expression, giving arguments for any generic type parameters.
    | PrimaryMethodCall (XMethodInvocation x) (Exp x) [NonWildTypeArgument x] (Ident x) [Argument x]
    -- | Invoking (XMethodInvocation x) method of the super class, giving arguments for any generic type parameters.
    | SuperMethodCall (XMethodInvocation x) [NonWildTypeArgument x] (Ident x) [Argument x]
    -- | Invoking (XMethodInvocation x) method of the superclass of (XMethodInvocation x) named class, giving arguments for any generic type parameters.
    | ClassMethodCall (XMethodInvocation x) (Name x) [NonWildTypeArgument x] (Ident x) [Argument x]
    -- | Invoking (XMethodInvocation x) method of (XMethodInvocation x) named type, giving arguments for any generic type parameters.
    | TypeMethodCall (XMethodInvocation x) (Name x) [NonWildTypeArgument x] (Ident x) [Argument x]
type family XMethodInvocation x

-- | An array initializer may be specified in a declaration, or as part of an array creation expression, creating an
--   array and providing some initial values
-- type families: XArrayInit
data ArrayInit x
    = ArrayInit (XArrayInit x) [VarInit x]
type family XArrayInit x

-----------------------------------------------------------------------
-- Types

data ReturnType x
    = VoidType (XReturnType x)
    | LockType (XReturnType x)
    | Type (XReturnType x) (Type x)
type family XReturnType x

-- | There are two kinds of types in the Java programming language: primitive types and reference types.
-- type families: XType
data Type x
    = PrimType (XType x) (PrimType x)
    | RefType (XType x) (RefType x)
    | AntiQType (XType x) String
type family XType x

-- | There are three kinds of reference types: class types, interface types, and array types.
--   Reference types may be parameterized with type arguments.
--   Type variables are introduced by generic type parameters.
-- type families: XRefType
data RefType x
    = ClassRefType (XRefType x) (ClassType x)
    | TypeVariable (XRefType x) (Ident x)
    | ArrayType    (XRefType x) (Type x) [Maybe (Policy x)]
    -- ^ The second argument to ArrayType is the base type, and should not be an array type
type family XRefType x

-- | A class or interface type consists of a type declaration specifier,
--   optionally followed by type arguments (in which case it is a parameterized type).
-- type families: XClassType
data ClassType x
    = ClassType(XClassType x) (Name x) [TypeArgument x]
type family XClassType x

-- | Type arguments may be either reference types or wildcards.
-- type families: XTypeArgument
data TypeArgument x
    = Wildcard  (XTypeArgument x) (Maybe (WildcardBound x))
    | ActualArg (XTypeArgument x) (NonWildTypeArgument x)
type family XTypeArgument x

data NonWildTypeArgument x
    = ActualName (XNonWildTypeArgument x) (Name x)      -- Can mean (XNonWildTypeArgument x) type or an exp
    | ActualType (XNonWildTypeArgument x) (RefType x)
    | ActualExp (XNonWildTypeArgument x) (Exp x)        -- Constrained to argExp
    | ActualLockState (XNonWildTypeArgument x) [Lock x]
type family XNonWildTypeArgument x


-- | Wildcards may be given explicit bounds, either upper (@extends@) or lower (@super@) bounds.
-- type families: XWildcardBound
data WildcardBound x
    = ExtendsBound (XWildcardBound x) (RefType x)
    | SuperBound (XWildcardBound x) (RefType x)
type family XWildcardBound x

-- | A primitive type is predefined by the Java programming language and named by its reserved keyword.
-- type families: XPrimType
data PrimType  x
    = BooleanT (XPrimType x)
    | ByteT    (XPrimType x)
    | ShortT   (XPrimType x)
    | IntT     (XPrimType x)
    | LongT    (XPrimType x)
    | CharT    (XPrimType x)
    | FloatT   (XPrimType x)
    | DoubleT  (XPrimType x)
-- Paragon
    | ActorT   (XPrimType x)
    | PolicyT  (XPrimType x)
type family XPrimType x


aPrimType :: (XPrimType x -> a) -> PrimType x -> a
aPrimType f (BooleanT x) = f x


-- Returns a value of whatever XPrimType is when indexed by x
aOfPrimType :: PrimType x -> XPrimType x
aOfPrimType (BooleanT a) = a
aOfPrimType (ByteT    a) = a
aOfPrimType (ShortT   a) = a
aOfPrimType (IntT     a) = a
aOfPrimType (LongT    a) = a
aOfPrimType (CharT    a) = a
aOfPrimType (FloatT   a) = a
aOfPrimType (DoubleT  a) = a
aOfPrimType (ActorT   a) = a
aOfPrimType (PolicyT  a) = a

-- | A class is generic if it declares one or more type variables. These type variables are known
--   as the type parameters of the class.
--   Paragon adds three new forms - actor, policy and lockstate parameters.
-- type families: XTypeParam
data TypeParam x = TypeParam (XTypeParam x) (Ident x) [RefType x]
-- Paragon
                 | ActorParam    (XTypeParam x) (RefType x) (Ident x)
                 | PolicyParam   (XTypeParam x) (Ident x)
                 | LockStateParam(XTypeParam x) (Ident x)
type family XTypeParam x

-----------------------------------------------------------------------
-- Paragon

type Policy a = Exp a

-- | A policy is a conjunction (set) of clauses, represented as a list.
--data PolicyLit = PolicyLit [Clause Actor]
-- type families : XPolicyExp
data PolicyExp x = PolicyLit     (XPolicyExp x) [Clause x]
                 | PolicyOf      (XPolicyExp x) (Ident x)
                 | PolicyThis    (XPolicyExp x)
                 | PolicyTypeVar (XPolicyExp x) (Ident x)
type family XPolicyExp x

-- | A lock property is a potentially recursive policy with an atom head.
-- type families: XLockProperties
data LockProperties x = LockProperties (XLockProperties x) [LClause x]
type family XLockProperties x

-- HERE
-- | A clause of the form Sigma => a, where a is an actor and Sigma a set of
--   locks/atomic predicates that must be open/true.
-- type families: XClause
data Clause x = Clause (XClause x) [ClauseVarDecl x] (ClauseHead x) [Atom x]
type family XClause x

-- | type families : XClauseVarDecl
data ClauseVarDecl x = ClauseVarDecl (XClauseVarDecl x) (RefType x) (Ident x)
type family XClauseVarDecl x

-- | type families : XClauseHead
data ClauseHead x = ClauseDeclHead (XClauseHead x) (ClauseVarDecl x)
                  | ClauseVarHead (XClauseHead x) (Actor x)
type family XClauseHead x

-- | type families: XLClause
data LClause x = LClause (XLClause x) [ClauseVarDecl x] (Atom x) [Atom x]
               | ConstraintClause (XLClause x) [ClauseVarDecl x] [Atom x]
type family XLClause x

-- | An actor variable, either forall-quantified within the current clause, or
--   free and thus concrete w.r.t. the policy under scrutiny.
-- type families: XActor
data Actor x = Actor (XActor x) (ActorName x)    -- ^ Free actor variables
             | Var   (XActor x) (Ident x)        -- ^ Quantified actor variables

type family XActor x

-- | Type families: XActorName
data ActorName x
    = ActorName (XActorName x) (Name x)
    -- ^ A free actor variable
    | ActorTypeVar (XActorName x) (RefType x) (Ident x)
    -- ^ A free actor type parameter
type family XActorName x


-- | A lock is an atomic n-ary predicate.
-- Type families: XAtom
data Atom x = Atom (XAtom x) (Name x) [Actor x]
type family XAtom x


-- | Type families: XLock
data Lock x = Lock (XLock x) (Name x) [ActorName x] | LockVar (XLock x) (Ident x)
type family XLock x

-----------------------------------------------------------------------
-- Useful accessors


importDeclName :: ImportDecl a -> Name a
importDeclName (SingleTypeImport    _ n)   = n
importDeclName (TypeImportOnDemand  _ n)   = n
importDeclName (SingleStaticImport  _ n _) = n
importDeclName (StaticImportOnDemand _ n)   = n


-----------------------------------------------------------------------
-- Names and identifiers

-- | A single identifier
-- Type families: XIdent
data Ident x = Ident (XIdent x) B.ByteString | AntiQIdent (XIdent x) String
type family XIdent x

-- | Extract actual identifier string from Ident wrapper type
unIdent :: Ident a -> B.ByteString
unIdent (Ident _ bs) = bs
unIdent (AntiQIdent _ str) = panic (syntaxModule ++ ".unIdent")
            $ "AntiQIdent " ++ str

-- | A name, i.e. a period-separated list of identifiers.
-- type families : XName
data Name x = Name (XName x) NameType (Maybe (Name x)) (Ident x)
            | AntiQName (XName x) String
   -- Show removed to get more readable debug output
type family XName x
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

-- Changed, see Syntax.hs for old impl.
mkSimpleName :: NameType -> Ident a -> Name a
mkSimpleName nt i =  Name (ann i) nt Nothing i

mkUniformName :: (XName a -> XName a -> XName a)
              -> NameType -> [Ident a] -> Name a
mkUniformName f nt ids = mkName' (reverse ids)
    where mkName' [] = panic (syntaxModule ++ ".mkUniformName")
                             "Empty list of idents"
          mkName' [i] = Name (ann i) nt Nothing i
          mkName' (i:is) =
              let pre = mkName' is
                  a = f (ann pre) (ann i)
              in Name a nt (Just pre) i
-- Changed, see Syntax.hs for old impl.
mkUniformName_ :: NameType -> [Ident a] -> Name a
mkUniformName_ = mkUniformName const

mkName :: (XName a -> XName a -> XName a) -> NameType
       -> NameType -> [Ident a] -> Name a
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

temp = error "The temp variable used for the old Name type was evaluated."


-- These will probably not be used anymore, commented out for now
-------------------------------------------------------
-- mkIdent :: XIdent a -> XSP a -> String -> Ident a --
-- mkIdent a sp = Ident a sp . B.pack                --
--                                                   --
-- mkIdent_ :: XIdent a -> String -> Ident a         --
-- mkIdent_ a = mkIdent a defaultPos                 --
-------------------------------------------------------

-----------------------------------------------------------------------
-- Annotations

-- $(deriveAnnMany
--   [''CompilationUnit, ''PackageDecl, ''ImportDecl,
--    ''TypeDecl, ''ClassDecl, ''ClassBody, ''EnumBody, ''EnumConstant,
--    ''InterfaceDecl, ''InterfaceBody, ''Decl, ''MemberDecl,
--    ''VarDecl, ''VarDeclId, ''VarInit, ''ArrayInit,
--    ''FormalParam, ''MethodBody, ''ConstructorBody, ''ExplConstrInv,
--    ''Modifier, ''Block, ''BlockStmt, ''Stmt,
--    ''Catch, ''SwitchBlock, ''SwitchLabel, ''ForInit, ''ExceptionSpec,
--    ''Exp, ''Literal, ''Op, ''AssignOp, ''Lhs,
--    ''ArrayIndex, ''FieldAccess, ''MethodInvocation,
--    ''Type, ''PrimType, ''RefType, ''ClassType, ''ReturnType,
--    ''TypeArgument, ''NonWildTypeArgument, ''WildcardBound, ''TypeParam,
--    ''PolicyExp, ''LockProperties, ''Clause, ''LClause,
--    ''ClauseVarDecl, ''ClauseHead,
--    ''Actor, ''ActorName, ''Atom, ''Lock, ''Ident, ''Name])

----------------------------------
-- INSTANCES

type ForallXFamilies (f :: * -> Constraint) x =
  (
    f x, f(XCompilationUnit x), f(XPackageDecl x), f(XImportDecl x),
    f(XImportDecl x), f(XTypeDecl x), f(XClassDecl x), f(XClassBody x),
        f(XEnumBody x), f(XEnumConstant x), f(XInterfaceDecl x),
        f(XInterfaceBody x), f(XDecl x), f(XMemberDecl x), f(XVarDecl x),
        f(XVarDeclId x), f(XFormalParam x), f(XMethodBody x),
        f(XConstructorBody x), f(XExplConstrInv x), f(XMod x), f(XBlock x),
        f(XBlockStm x), f(XStm x), f(XCatch x), f(XSwitchBlock x),
        f(XSwitchBlock x), f(XSwitchLabel x), f(XForInit x),
        f(XExceptionSpec x), f(XExp x), f(XLiteral x), f(XOp x),
    f(XAssignOp x), f(XLhs x), f(XArrayIndex x), f(XFieldAccess x),
        f(XMethodInvocation x), f(XArrayInit x), f(XReturnType x), f(XType x),
        f(XRefType x), f(XClassType x), f(XTypeArgument x),
        f(XNonWildTypeArgument x), f(XWildcardBound x), f(XPrimType x),
        f(XTypeParam x), f(XPolicyExp x), f(XLockProperties x), f(XClause x),
        f(XClauseVarDecl x), f(XClauseHead x), f(XLClause x), f(XActor x),
    f(XActorName x), f(XAtom x), f(XLock x), f(XIdent x), f(XVarDecl x),
        f(XVarInit x), f (XName x)
  )
type ForallIncData (f :: * -> Constraint) x = (ForallXFamilies f x, Data x, Typeable x)

deriving instance ForallXFamilies Show x => Show (CompilationUnit x)
deriving instance ForallXFamilies Show x => Show (PackageDecl x)
deriving instance ForallXFamilies Show x => Show (ImportDecl x)
deriving instance ForallXFamilies Show x => Show (TypeDecl x)
deriving instance ForallXFamilies Show x => Show (ClassDecl x)
deriving instance ForallXFamilies Show x => Show (ClassBody x)
deriving instance ForallXFamilies Show x => Show (EnumBody x)
deriving instance ForallXFamilies Show x => Show (EnumConstant x)
deriving instance ForallXFamilies Show x => Show (InterfaceDecl x)
deriving instance ForallXFamilies Show x => Show (InterfaceBody x)
deriving instance ForallXFamilies Show x => Show (Decl x)
deriving instance ForallXFamilies Show x => Show (MemberDecl x)
deriving instance ForallXFamilies Show x => Show (VarDecl x)
deriving instance ForallXFamilies Show x => Show (VarDeclId x)
deriving instance ForallXFamilies Show x => Show (VarInit x)
deriving instance ForallXFamilies Show x => Show (FormalParam x)
deriving instance ForallXFamilies Show x => Show (MethodBody x)
deriving instance ForallXFamilies Show x => Show (ConstructorBody x)
deriving instance ForallXFamilies Show x => Show (ExplConstrInv x)
deriving instance ForallXFamilies Show x => Show (Modifier x)
deriving instance ForallXFamilies Show x => Show (Block x)
deriving instance ForallXFamilies Show x => Show (BlockStmt x)
deriving instance ForallXFamilies Show x => Show (Stmt x)
deriving instance ForallXFamilies Show x => Show (Catch x)
deriving instance ForallXFamilies Show x => Show (SwitchBlock x)
deriving instance ForallXFamilies Show x => Show (SwitchLabel x)
deriving instance ForallXFamilies Show x => Show (ForInit x)
deriving instance ForallXFamilies Show x => Show (ExceptionSpec x)
deriving instance ForallXFamilies Show x => Show (Exp x)
deriving instance ForallXFamilies Show x => Show (Literal x)
deriving instance ForallXFamilies Show x => Show (Op x)
deriving instance ForallXFamilies Show x => Show (AssignOp x)
deriving instance ForallXFamilies Show x => Show (Lhs x)
deriving instance ForallXFamilies Show x => Show (ArrayIndex x)
deriving instance ForallXFamilies Show x => Show (FieldAccess x)
deriving instance ForallXFamilies Show x => Show (MethodInvocation x)
deriving instance ForallXFamilies Show x => Show (ArrayInit x)
deriving instance ForallXFamilies Show x => Show (ReturnType x)
deriving instance ForallXFamilies Show x => Show (Type x)
deriving instance ForallXFamilies Show x => Show (RefType x)
deriving instance ForallXFamilies Show x => Show (ClassType x)
deriving instance ForallXFamilies Show x => Show (TypeArgument x)
deriving instance ForallXFamilies Show x => Show (NonWildTypeArgument x)
deriving instance ForallXFamilies Show x => Show (WildcardBound x)
deriving instance ForallXFamilies Show x => Show (PrimType x)
deriving instance ForallXFamilies Show x => Show (TypeParam x)
deriving instance ForallXFamilies Show x => Show (PolicyExp x)
deriving instance ForallXFamilies Show x => Show (LockProperties x)
deriving instance ForallXFamilies Show x => Show (Clause x)
deriving instance ForallXFamilies Show x => Show (ClauseVarDecl x)
deriving instance ForallXFamilies Show x => Show (ClauseHead x)
deriving instance ForallXFamilies Show x => Show (LClause x)
deriving instance ForallXFamilies Show x => Show (Actor x)
deriving instance ForallXFamilies Show x => Show (ActorName x)
deriving instance ForallXFamilies Show x => Show (Atom x)
deriving instance ForallXFamilies Show x => Show (Lock x)
deriving instance ForallXFamilies Show x => Show (Ident x)



deriving instance ForallXFamilies Eq x => Eq (CompilationUnit x)
deriving instance ForallXFamilies Eq x => Eq (PackageDecl x)
deriving instance ForallXFamilies Eq x => Eq (ImportDecl x)
deriving instance ForallXFamilies Eq x => Eq (TypeDecl x)
deriving instance ForallXFamilies Eq x => Eq (ClassDecl x)
deriving instance ForallXFamilies Eq x => Eq (ClassBody x)
deriving instance ForallXFamilies Eq x => Eq (EnumBody x)
deriving instance ForallXFamilies Eq x => Eq (EnumConstant x)
deriving instance ForallXFamilies Eq x => Eq (InterfaceDecl x)
deriving instance ForallXFamilies Eq x => Eq (InterfaceBody x)
deriving instance ForallXFamilies Eq x => Eq (Decl x)
deriving instance ForallXFamilies Eq x => Eq (MemberDecl x)
deriving instance ForallXFamilies Eq x => Eq (VarDecl x)
deriving instance ForallXFamilies Eq x => Eq (VarDeclId x)
deriving instance ForallXFamilies Eq x => Eq (VarInit x)
deriving instance ForallXFamilies Eq x => Eq (FormalParam x)
deriving instance ForallXFamilies Eq x => Eq (MethodBody x)
deriving instance ForallXFamilies Eq x => Eq (ConstructorBody x)
deriving instance ForallXFamilies Eq x => Eq (ExplConstrInv x)
deriving instance ForallXFamilies Eq x => Eq (Modifier x)
deriving instance ForallXFamilies Eq x => Eq (Block x)
deriving instance ForallXFamilies Eq x => Eq (BlockStmt x)
deriving instance ForallXFamilies Eq x => Eq (Stmt x)
deriving instance ForallXFamilies Eq x => Eq (Catch x)
deriving instance ForallXFamilies Eq x => Eq (SwitchBlock x)
deriving instance ForallXFamilies Eq x => Eq (SwitchLabel x)
deriving instance ForallXFamilies Eq x => Eq (ForInit x)
deriving instance ForallXFamilies Eq x => Eq (ExceptionSpec x)
deriving instance ForallXFamilies Eq x => Eq (Exp x)
deriving instance ForallXFamilies Eq x => Eq (Literal x)
deriving instance ForallXFamilies Eq x => Eq (Op x)
deriving instance ForallXFamilies Eq x => Eq (AssignOp x)
deriving instance ForallXFamilies Eq x => Eq (Lhs x)
deriving instance ForallXFamilies Eq x => Eq (ArrayIndex x)
deriving instance ForallXFamilies Eq x => Eq (FieldAccess x)
deriving instance ForallXFamilies Eq x => Eq (MethodInvocation x)
deriving instance ForallXFamilies Eq x => Eq (ArrayInit x)
deriving instance ForallXFamilies Eq x => Eq (ReturnType x)
deriving instance ForallXFamilies Eq x => Eq (Type x)
deriving instance ForallXFamilies Eq x => Eq (RefType x)
deriving instance ForallXFamilies Eq x => Eq (ClassType x)
deriving instance ForallXFamilies Eq x => Eq (TypeArgument x)
deriving instance ForallXFamilies Eq x => Eq (NonWildTypeArgument x)
deriving instance ForallXFamilies Eq x => Eq (WildcardBound x)
deriving instance ForallXFamilies Eq x => Eq (PrimType x)
deriving instance ForallXFamilies Eq x => Eq (TypeParam x)
deriving instance ForallXFamilies Eq x => Eq (PolicyExp x)
deriving instance ForallXFamilies Eq x => Eq (LockProperties x)
deriving instance ForallXFamilies Eq x => Eq (Clause x)
deriving instance ForallXFamilies Eq x => Eq (ClauseVarDecl x)
deriving instance ForallXFamilies Eq x => Eq (ClauseHead x)
deriving instance ForallXFamilies Eq x => Eq (LClause x)
deriving instance ForallXFamilies Eq x => Eq (Actor x)
deriving instance ForallXFamilies Eq x => Eq (ActorName x)
deriving instance ForallXFamilies Eq x => Eq (Atom x)
deriving instance ForallXFamilies Eq x => Eq (Lock x)
deriving instance ForallXFamilies Eq x => Eq (Ident x)
deriving instance ForallXFamilies Eq x => Eq (Name x)

deriving instance ForallXFamilies Ord x => Ord (CompilationUnit x)
deriving instance ForallXFamilies Ord x => Ord (PackageDecl x)
deriving instance ForallXFamilies Ord x => Ord (ImportDecl x)
deriving instance ForallXFamilies Ord x => Ord (TypeDecl x)
deriving instance ForallXFamilies Ord x => Ord (ClassDecl x)
deriving instance ForallXFamilies Ord x => Ord (ClassBody x)
deriving instance ForallXFamilies Ord x => Ord (EnumBody x)
deriving instance ForallXFamilies Ord x => Ord (EnumConstant x)
deriving instance ForallXFamilies Ord x => Ord (InterfaceDecl x)
deriving instance ForallXFamilies Ord x => Ord (InterfaceBody x)
deriving instance ForallXFamilies Ord x => Ord (Decl x)
deriving instance ForallXFamilies Ord x => Ord (MemberDecl x)
deriving instance ForallXFamilies Ord x => Ord (VarDecl x)
deriving instance ForallXFamilies Ord x => Ord (VarDeclId x)
deriving instance ForallXFamilies Ord x => Ord (VarInit x)
deriving instance ForallXFamilies Ord x => Ord (FormalParam x)
deriving instance ForallXFamilies Ord x => Ord (MethodBody x)
deriving instance ForallXFamilies Ord x => Ord (ConstructorBody x)
deriving instance ForallXFamilies Ord x => Ord (ExplConstrInv x)
deriving instance ForallXFamilies Ord x => Ord (Modifier x)
deriving instance ForallXFamilies Ord x => Ord (Block x)
deriving instance ForallXFamilies Ord x => Ord (BlockStmt x)
deriving instance ForallXFamilies Ord x => Ord (Stmt x)
deriving instance ForallXFamilies Ord x => Ord (Catch x)
deriving instance ForallXFamilies Ord x => Ord (SwitchBlock x)
deriving instance ForallXFamilies Ord x => Ord (SwitchLabel x)
deriving instance ForallXFamilies Ord x => Ord (ForInit x)
deriving instance ForallXFamilies Ord x => Ord (ExceptionSpec x)
deriving instance ForallXFamilies Ord x => Ord (Exp x)
deriving instance ForallXFamilies Ord x => Ord (Literal x)
deriving instance ForallXFamilies Ord x => Ord (Op x)
deriving instance ForallXFamilies Ord x => Ord (AssignOp x)
deriving instance ForallXFamilies Ord x => Ord (Lhs x)
deriving instance ForallXFamilies Ord x => Ord (ArrayIndex x)
deriving instance ForallXFamilies Ord x => Ord (FieldAccess x)
deriving instance ForallXFamilies Ord x => Ord (MethodInvocation x)
deriving instance ForallXFamilies Ord x => Ord (ArrayInit x)
deriving instance ForallXFamilies Ord x => Ord (ReturnType x)
deriving instance ForallXFamilies Ord x => Ord (Type x)
deriving instance ForallXFamilies Ord x => Ord (RefType x)
deriving instance ForallXFamilies Ord x => Ord (ClassType x)
deriving instance ForallXFamilies Ord x => Ord (TypeArgument x)
deriving instance ForallXFamilies Ord x => Ord (NonWildTypeArgument x)
deriving instance ForallXFamilies Ord x => Ord (WildcardBound x)
deriving instance ForallXFamilies Ord x => Ord (PrimType x)
deriving instance ForallXFamilies Ord x => Ord (TypeParam x)
deriving instance ForallXFamilies Ord x => Ord (PolicyExp x)
deriving instance ForallXFamilies Ord x => Ord (LockProperties x)
deriving instance ForallXFamilies Ord x => Ord (Clause x)
deriving instance ForallXFamilies Ord x => Ord (ClauseVarDecl x)
deriving instance ForallXFamilies Ord x => Ord (ClauseHead x)
deriving instance ForallXFamilies Ord x => Ord (LClause x)
deriving instance ForallXFamilies Ord x => Ord (Actor x)
deriving instance ForallXFamilies Ord x => Ord (ActorName x)
deriving instance ForallXFamilies Ord x => Ord (Atom x)
deriving instance ForallXFamilies Ord x => Ord (Lock x)
deriving instance ForallXFamilies Ord x => Ord (Ident x)
deriving instance ForallXFamilies Ord x => Ord (Name x)

deriving instance ForallIncData Data x => Data (CompilationUnit x)
deriving instance ForallIncData Data x => Data (PackageDecl x)
deriving instance ForallIncData Data x => Data (ImportDecl x)
deriving instance ForallIncData Data x => Data (TypeDecl x)
deriving instance ForallIncData Data x => Data (ClassDecl x)
deriving instance ForallIncData Data x => Data (ClassBody x)
deriving instance ForallIncData Data x => Data (EnumBody x)
deriving instance ForallIncData Data x => Data (EnumConstant x)
deriving instance ForallIncData Data x => Data (InterfaceDecl x)
deriving instance ForallIncData Data x => Data (InterfaceBody x)
deriving instance ForallIncData Data x => Data (Decl x)
deriving instance ForallIncData Data x => Data (MemberDecl x)
deriving instance ForallIncData Data x => Data (VarDecl x)
deriving instance ForallIncData Data x => Data (VarDeclId x)
deriving instance ForallIncData Data x => Data (VarInit x)
deriving instance ForallIncData Data x => Data (FormalParam x)
deriving instance ForallIncData Data x => Data (MethodBody x)
deriving instance ForallIncData Data x => Data (ConstructorBody x)
deriving instance ForallIncData Data x => Data (ExplConstrInv x)
deriving instance ForallIncData Data x => Data (Modifier x)
deriving instance ForallIncData Data x => Data (Block x)
deriving instance ForallIncData Data x => Data (BlockStmt x)
deriving instance ForallIncData Data x => Data (Stmt x)
deriving instance ForallIncData Data x => Data (Catch x)
deriving instance ForallIncData Data x => Data (SwitchBlock x)
deriving instance ForallIncData Data x => Data (SwitchLabel x)
deriving instance ForallIncData Data x => Data (ForInit x)
deriving instance ForallIncData Data x => Data (ExceptionSpec x)
deriving instance ForallIncData Data x => Data (Exp x)
deriving instance ForallIncData Data x => Data (Literal x)
deriving instance ForallIncData Data x => Data (Op x)
deriving instance ForallIncData Data x => Data (AssignOp x)
deriving instance ForallIncData Data x => Data (Lhs x)
deriving instance ForallIncData Data x => Data (ArrayIndex x)
deriving instance ForallIncData Data x => Data (FieldAccess x)
deriving instance ForallIncData Data x => Data (MethodInvocation x)
deriving instance ForallIncData Data x => Data (ArrayInit x)
deriving instance ForallIncData Data x => Data (ReturnType x)
deriving instance ForallIncData Data x => Data (Type x)
deriving instance ForallIncData Data x => Data (RefType x)
deriving instance ForallIncData Data x => Data (ClassType x)
deriving instance ForallIncData Data x => Data (TypeArgument x)
deriving instance ForallIncData Data x => Data (NonWildTypeArgument x)
deriving instance ForallIncData Data x => Data (WildcardBound x)
deriving instance ForallIncData Data x => Data (PrimType x)
deriving instance ForallIncData Data x => Data (TypeParam x)
deriving instance ForallIncData Data x => Data (PolicyExp x)
deriving instance ForallIncData Data x => Data (LockProperties x)
deriving instance ForallIncData Data x => Data (Clause x)
deriving instance ForallIncData Data x => Data (ClauseVarDecl x)
deriving instance ForallIncData Data x => Data (ClauseHead x)
deriving instance ForallIncData Data x => Data (LClause x)
deriving instance ForallIncData Data x => Data (Actor x)
deriving instance ForallIncData Data x => Data (ActorName x)
deriving instance ForallIncData Data x => Data (Atom x)
deriving instance ForallIncData Data x => Data (Lock x)
deriving instance ForallIncData Data x => Data (Ident x)
deriving instance ForallIncData Data x => Data (Name x)


deriving instance ForallIncData Typeable x => Typeable (CompilationUnit x)
deriving instance ForallIncData Typeable x => Typeable (PackageDecl x)
deriving instance ForallIncData Typeable x => Typeable (ImportDecl x)
deriving instance ForallIncData Typeable x => Typeable (TypeDecl x)
deriving instance ForallIncData Typeable x => Typeable (ClassDecl x)
deriving instance ForallIncData Typeable x => Typeable (ClassBody x)
deriving instance ForallIncData Typeable x => Typeable (EnumBody x)
deriving instance ForallIncData Typeable x => Typeable (EnumConstant x)
deriving instance ForallIncData Typeable x => Typeable (InterfaceDecl x)
deriving instance ForallIncData Typeable x => Typeable (InterfaceBody x)
deriving instance ForallIncData Typeable x => Typeable (Decl x)
deriving instance ForallIncData Typeable x => Typeable (MemberDecl x)
deriving instance ForallIncData Typeable x => Typeable (VarDecl x)
deriving instance ForallIncData Typeable x => Typeable (VarDeclId x)
deriving instance ForallIncData Typeable x => Typeable (VarInit x)
deriving instance ForallIncData Typeable x => Typeable (FormalParam x)
deriving instance ForallIncData Typeable x => Typeable (MethodBody x)
deriving instance ForallIncData Typeable x => Typeable (ConstructorBody x)
deriving instance ForallIncData Typeable x => Typeable (ExplConstrInv x)
deriving instance ForallIncData Typeable x => Typeable (Modifier x)
deriving instance ForallIncData Typeable x => Typeable (Block x)
deriving instance ForallIncData Typeable x => Typeable (BlockStmt x)
deriving instance ForallIncData Typeable x => Typeable (Stmt x)
deriving instance ForallIncData Typeable x => Typeable (Catch x)
deriving instance ForallIncData Typeable x => Typeable (SwitchBlock x)
deriving instance ForallIncData Typeable x => Typeable (SwitchLabel x)
deriving instance ForallIncData Typeable x => Typeable (ForInit x)
deriving instance ForallIncData Typeable x => Typeable (ExceptionSpec x)
deriving instance ForallIncData Typeable x => Typeable (Exp x)
deriving instance ForallIncData Typeable x => Typeable (Literal x)
deriving instance ForallIncData Typeable x => Typeable (Op x)
deriving instance ForallIncData Typeable x => Typeable (AssignOp x)
deriving instance ForallIncData Typeable x => Typeable (Lhs x)
deriving instance ForallIncData Typeable x => Typeable (ArrayIndex x)
deriving instance ForallIncData Typeable x => Typeable (FieldAccess x)
deriving instance ForallIncData Typeable x => Typeable (MethodInvocation x)
deriving instance ForallIncData Typeable x => Typeable (ArrayInit x)
deriving instance ForallIncData Typeable x => Typeable (ReturnType x)
deriving instance ForallIncData Typeable x => Typeable (Type x)
deriving instance ForallIncData Typeable x => Typeable (RefType x)
deriving instance ForallIncData Typeable x => Typeable (ClassType x)
deriving instance ForallIncData Typeable x => Typeable (TypeArgument x)
deriving instance ForallIncData Typeable x => Typeable (NonWildTypeArgument x)
deriving instance ForallIncData Typeable x => Typeable (WildcardBound x)
deriving instance ForallIncData Typeable x => Typeable (PrimType x)
deriving instance ForallIncData Typeable x => Typeable (TypeParam x)
deriving instance ForallIncData Typeable x => Typeable (PolicyExp x)
deriving instance ForallIncData Typeable x => Typeable (LockProperties x)
deriving instance ForallIncData Typeable x => Typeable (Clause x)
deriving instance ForallIncData Typeable x => Typeable (ClauseVarDecl x)
deriving instance ForallIncData Typeable x => Typeable (ClauseHead x)
deriving instance ForallIncData Typeable x => Typeable (LClause x)
deriving instance ForallIncData Typeable x => Typeable (Actor x)
deriving instance ForallIncData Typeable x => Typeable (ActorName x)
deriving instance ForallIncData Typeable x => Typeable (Atom x)
deriving instance ForallIncData Typeable x => Typeable (Lock x)
deriving instance ForallIncData Typeable x => Typeable (Ident x)
deriving instance ForallIncData Typeable x => Typeable (Name x)


-- deriving instance Functor CompilationUnit
-- deriving instance Functor PackageDecl
-- deriving instance Functor ImportDecl
-- deriving instance Functor TypeDecl
-- deriving instance Functor ClassDecl
-- deriving instance Functor ClassBody
-- deriving instance Functor EnumBody
-- deriving instance Functor EnumConstant
-- deriving instance Functor InterfaceDecl
-- deriving instance Functor InterfaceBody
-- deriving instance Functor Decl
-- deriving instance Functor MemberDecl
-- deriving instance Functor VarDecl
-- deriving instance Functor VarDeclId
-- deriving instance Functor VarInit
-- deriving instance Functor FormalParam
-- deriving instance Functor MethodBody
-- deriving instance Functor ConstructorBody
-- deriving instance Functor ExplConstrInv
-- deriving instance Functor Modifier
-- deriving instance Functor Block
-- deriving instance Functor BlockStmt
-- deriving instance Functor Stmt
-- deriving instance Functor Catch
-- deriving instance Functor SwitchBlock
-- deriving instance Functor SwitchLabel
-- deriving instance Functor ForInit
-- deriving instance Functor ExceptionSpec
-- deriving instance Functor Exp
-- deriving instance Functor Literal
-- deriving instance Functor Op
-- deriving instance Functor AssignOp
-- deriving instance Functor Lhs
-- deriving instance Functor ArrayIndex
-- deriving instance Functor FieldAccess
-- deriving instance Functor MethodInvocation
-- deriving instance Functor ArrayInit
-- deriving instance Functor ReturnType
-- deriving instance Functor Type
-- deriving instance Functor RefType
-- deriving instance Functor ClassType
-- deriving instance Functor TypeArgument
-- deriving instance Functor NonWildTypeArgument
-- deriving instance Functor WildcardBound
-- deriving instance Functor PrimType
-- deriving instance Functor TypeParam
-- deriving instance Functor PolicyExp
-- deriving instance Functor LockProperties
-- deriving instance Functor Clause
-- deriving instance Functor ClauseVarDecl
-- deriving instance Functor ClauseHead
-- deriving instance Functor LClause
-- deriving instance Functor Actor
-- deriving instance Functor ActorName
-- deriving instance Functor Atom
-- deriving instance Functor Lock
-- deriving instance Functor Ident
-- deriving instance Functor Name

--  Prints name as a simple string to be easier to read.
-- To get printout of the whole recursive name structure, comment this out and put
-- Show in the deriving clause.
instance Show (Name a) where
  show (Name _ _ nextBase (Ident _ iBase)) =
    show (showInner nextBase ++ B.unpack iBase)
    where
      showInner Nothing = ""
      showInner (Just (Name _ _ next (Ident _ i))) =  showInner next ++ B.unpack i ++ "."
