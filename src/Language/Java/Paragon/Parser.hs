{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE CPP, PatternGuards, TupleSections #-}
{-# LANGUAGE StandaloneDeriving, FlexibleInstances #-}
module Language.Java.Paragon.Parser (
    parser,

    compilationUnit, packageDecl, importDecl, typeDecl,

    classDecl, interfaceDecl,

    memberDecl, fieldDecl, methodDecl, constrDecl,
    interfaceMemberDecl, absMethodDecl, lockDecl,


    methodBody, formalParams, formalParam,

    modifier,

    varDecls, varDecl, varInit, arrayInit,

    block, blockStmt, stmt,

    stmtExp, exp, primary, literal, lhs,

    ttype, primType, refType, classType, returnType,

    typeParams, typeParam,

    name, ident,
    nameRaw, ambName, eName, tName, pName, pOrTName, mOrLName,
    flattenName,

    policy, policyExp, clause, actor, atom, lock, lockProperties,


    empty, list, list1, seplist, seplist1, opt, bopt, lopt,

    comma, semiColon, period, colon,

    ParseError

    -- module Text.ParserCombinators.Parsec.Pos

    ) where

import Language.Java.Paragon.Lexer (L(..), Token(..), lexer)
import Language.Java.Paragon.SyntaxTTG as AST
import Language.Java.Paragon.Decorations.PaDecoration as D
import Language.Java.Paragon.Decorations.DecorationTypes
import Language.Java.Paragon.Decorations.PcsDecoration (PCS) -- TODO: remove, this is a hack

-- import Language.Java.Paragon.Syntax
import Language.Java.Paragon.Pretty (prettyPrint)
import Language.Java.Paragon.Interaction
import Text.ParserCombinators.Parsec hiding (SourcePos)
import Language.Java.Paragon.SourcePos

import Prelude hiding (exp, (>>), (>>=))
import qualified Prelude as P ((>>), (>>=))
import qualified Data.ByteString.Char8 as B
import Data.Maybe (isJust, catMaybes, fromJust)
import Control.Applicative (Applicative(..), (<$>), (<*>))
import Control.Monad (unless)

import Data.Generics.Uniplate.Data
#ifdef BASE4
import Data.Data
#else
import Data.Generics (Data(..),Typeable(..))
#endif


parserModule :: String
parserModule = libraryBase ++ ".Parser"

type P = GenParser (L Token) ()

-- A trick to allow >> and >>=, normally infixr 1, to be
-- used inside branches of <|>, which is declared as infixl 1.
-- There are no clashes with other operators of precedence 2.
(>>) :: Monad m => m a -> m b -> m b
(>>) = (P.>>)
(>>=) :: Monad m => m a -> (a -> m b) -> m b
(>>=) = (P.>>=)
infixr 2 >>, >>=
-- Note also when reading that <$> is infixl 4 and thus has
-- lower precedence than all the others (>>, >>=, and <|>).

-- | Apply the given (constructor) function to the current position of the
-- parser
setPos :: (SourcePos -> b) -> P b
setPos x = getPosition >>= \pos -> return $ x (parSecToSourcePos pos)

-- ArrayInit :: SourcePos -> ArrayInit Pa -> VarInit Pa
----------------------------------------------------------------------------
-- Top-level parsing

parser :: P a -> String -> Either ParseError a
parser p = runParser p () "" . lexer

-- | Convert a parsec SourcePos to a paragon SourcePos
getParaPosition :: P SourcePos
getParaPosition = do pos <- getPosition
                     return (parSecToSourcePos pos)

----------------------------------------------------------------------------
-- Packages and compilation units

compilationUnit :: P (CompilationUnit Pa)
compilationUnit = setPos CompilationUnit
    <*> opt packageDecl
    <*> list importDecl
    <*> fmap catMaybes (list typeDecl)

packageDecl :: P (PackageDecl Pa)
packageDecl = do
    pos <- getParaPosition
    tok KW_Package
    n <- nameRaw pName
    semiColon
    return $ PackageDecl pos n

importDecl :: P (ImportDecl Pa)
importDecl = do
    pos <- getParaPosition
    tok KW_Import
    st <- bopt $ tok KW_Static
    n  <- nameRaw ambName
    ds <- bopt $ period >> tok Op_Star
    semiColon
    return $ mkImportDecl pos st ds n

  where mkImportDecl pos False False n
            = SingleTypeImport pos $ flattenRealName tName n
        mkImportDecl pos False True  n
            = TypeImportOnDemand pos $ flattenRealName pOrTName n
        mkImportDecl pos True  True  n
            = StaticImportOnDemand pos $ flattenRealName tName n
        mkImportDecl pos True  False n@Name{} =
            let is = flattenName n
            in case reverse is of
                 [] -> panic (parserModule ++ ".importDecl") "Empty name"
                 (lastI:initN) ->
                     SingleStaticImport pos
                       (tName $ reverse initN) lastI
        mkImportDecl _ _ _ _
            = error "Single static import declaration \
                    \requires at least one non-antiquote ident"

        flattenRealName rebuild n@Name{} = rebuild $ flattenName n
        flattenRealName _ n = n

typeDecl :: P (Maybe (TypeDecl Pa))
typeDecl = Just <$> classOrInterfaceDecl <|>
            const Nothing <$> semiColon

----------------------------------------------------------------------------
-- Declarations

-- Class declarations

classOrInterfaceDecl :: P (TypeDecl Pa)
classOrInterfaceDecl = unMod $
    (do pos <- getParaPosition
        cdecl <- classDeclM
        checkConstrs (cdecl [])
        return $ \ms -> ClassTypeDecl pos (cdecl ms)) <|>
    (do pos <- getParaPosition
        idecl <- interfaceDeclM
        return $ \ms -> InterfaceTypeDecl pos (idecl ms))

classDeclM :: P (Mod (ClassDecl Pa))
classDeclM = normalClassDeclM <|> enumClassDeclM

-- Not called internally:
-- | Top-level parser for class declarations
classDecl :: P (ClassDecl Pa)
classDecl = unMod classDeclM

unMod :: P (Mod a) -> P a
unMod pma = do
  ms <- list modifier
  pa <- pma
  return $ pa ms

normalClassDeclM :: P (Mod (ClassDecl Pa))
normalClassDeclM = do
    pos <- getParaPosition
    tok KW_Class
    i   <- ident
    tps <- lopt typeParams
    mex <- opt extends
    imp <- lopt implements
    bod <- classBody
    return $ \ms ->
        generalize tps $ ClassDecl pos ms i tps (fmap head mex) imp bod

extends :: P [ClassType Pa]
extends = tok KW_Extends >> classTypeList

implements :: P [ClassType Pa]
implements = tok KW_Implements >> classTypeList

enumClassDeclM :: P (Mod (ClassDecl Pa))
enumClassDeclM = do
    pos <- getParaPosition
    tok KW_Enum
    i   <- ident
    imp <- lopt implements
    bod <- enumBody
    return $ \ms -> EnumDecl pos ms i imp bod

classBody :: P (ClassBody Pa)
classBody = setPos ClassBody <*> braces classBodyDecls

enumBody :: P (EnumBody Pa)
enumBody = braces $ do
    pos <- getParaPosition
    ecs <- seplist enumConst comma
    optional comma
    eds <- lopt enumBodyDecls
    return $ EnumBody pos ecs eds

enumConst :: P (EnumConstant Pa)
enumConst = setPos EnumConstant
    <*> ident
    <*> lopt args
    <*> opt classBody

enumBodyDecls :: P [Decl Pa]
enumBodyDecls = semiColon >> classBodyDecls

classBodyDecls :: P [Decl Pa]
classBodyDecls = list classBodyDecl

-- Interface declarations

-- Not used internally:
-- | Top-level parser for interface declarations
interfaceDecl :: P (InterfaceDecl Pa)
interfaceDecl = unMod interfaceDeclM

interfaceDeclM :: P (Mod (InterfaceDecl Pa))
interfaceDeclM = {- trace "interfaceDeclM" $ -} do
    pos <- getParaPosition
    tok KW_Interface
    i   <- ident
    tps <- lopt typeParams
    exs <- lopt extends
    bod <- interfaceBody
    return $ \ms ->
        generalize tps $ InterfaceDecl pos ms i tps exs bod

interfaceBody :: P (InterfaceBody Pa)
interfaceBody = do pos <- getParaPosition
                   InterfaceBody pos . catMaybes <$>
                      braces (list interfaceBodyDecl)

-- Declarations

classBodyDecl :: P (Decl Pa)
classBodyDecl =
    (try $ setPos InitDecl
                <*> bopt (tok KW_Static)
                <*> block) <|>
    (do pos <- getParaPosition
        ms  <- list modifier
        dec <- memberDeclM
        return $ MemberDecl pos (dec ms))

-- Not used internally:
-- | Top-level parser for member declarations
memberDecl :: P (MemberDecl Pa)
memberDecl = unMod memberDeclM

memberDeclM :: P (Mod (MemberDecl Pa))
memberDeclM = {- trace "memberDeclM" $ -}
    (try $ do
        pos <- getParaPosition
        cd  <- classDeclM
        return $ \ms -> MemberClassDecl pos (cd ms)) <|>
    (try $ do
        pos <- getParaPosition
        idecl  <- interfaceDeclM
        return $ \ms -> MemberInterfaceDecl pos (idecl ms)) <|>
    try (fieldDeclM varDecl) <|>
    lockDeclM <|>        -- Paragon
--    policyDeclM <|>      -- Paragon
    try methodDeclM <|>
    constrDeclM

-- Not used internally:
-- | Top-level parser for field declarations
fieldDecl :: P (MemberDecl Pa)
fieldDecl = unMod (fieldDeclM varDecl)

fieldDeclM :: P (VarDecl Pa) -> P (Mod (MemberDecl Pa))
fieldDeclM varDeclFun = endSemi $ do
    pos <- getParaPosition
    typ <- ttype
    vds <- varDecls varDeclFun
    return $ \ms -> FieldDecl pos ms typ vds

interfaceFieldDeclM :: P (Mod (MemberDecl Pa))
interfaceFieldDeclM = fieldDeclM interfaceVarDecl

-- Not used internally:
-- | Top-level parser for method declarations
methodDecl :: P (MemberDecl Pa)
methodDecl = unMod methodDeclM

methodDeclM :: P (Mod (MemberDecl Pa))
methodDeclM = do
    pos <- getParaPosition
    tps <- lopt typeParams
    rt  <- returnType
    i   <- ident
    fps <- formalParams
    thr <- lopt throws
    bod <- methodBody
    return $ \ms ->
        generalize tps $ MethodDecl pos ms tps rt i fps thr bod

methodBody :: P (MethodBody Pa)
methodBody = setPos MethodBody <*>
    (const Nothing <$> semiColon <|> Just <$> block)

-- Not used internally:
-- | Top-level parser for constructor declarations
constrDecl :: P (MemberDecl Pa)
constrDecl = unMod constrDeclM

constrDeclM :: P (Mod (MemberDecl Pa))
constrDeclM = do
    pos <- getParaPosition
    tps <- lopt typeParams
    i   <- ident
    fps <- formalParams
    thr <- lopt throws
    bod <- constrBody
    return $ \ms ->
        generalize tps $ ConstructorDecl pos ms tps i fps thr bod

-- Not used internally:
-- | Top-level parser for lock declarations
lockDecl :: P (MemberDecl Pa)
lockDecl = unMod lockDeclM

lockDeclM :: P (Mod (MemberDecl Pa))
lockDeclM = endSemi $ do
    pos <- getParaPosition
    tok KW_P_Lock
    lc  <- ident
    ar  <- lopt arity
    lp  <- opt lockProperties
    return $ \ms -> LockDecl pos ms lc ar lp

arity :: P [RefType Pa]
arity = parens $ seplist refType comma

{-
policyDeclM :: P (Mod MemberDecl)
policyDeclM = endSemi $ do
    tok KW_P_Policy
    pn  <- ident
    tok Op_Equal
    pol <- policy
    return $ \ms -> PolicyDecl ms pn pol
-}

constrBody :: P (ConstructorBody Pa)
constrBody = braces $ do setPos ConstructorBody <*>
                           opt (try explConstrInv) <*>
                           list blockStmt

explConstrInv :: P (ExplConstrInv Pa)
explConstrInv = endSemi $
    (try $ do
        pos <- getParaPosition
        tas <- lopt nonWildTypeArgs
        tok KW_This
        as  <- args
        return $ ThisInvoke pos tas as) <|>
    (try $ do
        pos <- getParaPosition
        tas <- lopt nonWildTypeArgs
        tok KW_Super
        as  <- args
        return $ SuperInvoke pos tas as) <|>
    (do pos <- getParaPosition
        pri <- primary
        period
        tas <- lopt nonWildTypeArgs
        tok KW_Super
        as  <- args
        return $ PrimarySuperInvoke pos pri tas as)

-- TODO: This should be parsed like class bodies, and post-checked.
--       That would give far better error messages.
interfaceBodyDecl :: P (Maybe (MemberDecl Pa))
interfaceBodyDecl = semiColon >> return Nothing <|>
    do ms  <- list modifier
       imd <- interfaceMemberDeclM
       return $ Just (imd ms)

-- Not used internally:
-- | Top-level parser for interface member declarations
interfaceMemberDecl :: P (MemberDecl Pa)
interfaceMemberDecl = unMod interfaceMemberDeclM

interfaceMemberDeclM :: P (Mod (MemberDecl Pa))
interfaceMemberDeclM =
    (do pos <- getParaPosition
        cd  <- classDeclM
        return $ \ms -> MemberClassDecl pos (cd ms)) <|>
    (do pos <- getParaPosition
        idecl  <- interfaceDeclM
        return $ \ms -> MemberInterfaceDecl pos (idecl ms)) <|>
    try interfaceFieldDeclM <|>
    lockDeclM <|>
    absMethodDeclM

-- Not used internally:
-- | Top-level parser for abstract method declarations
absMethodDecl :: P (MemberDecl Pa)
absMethodDecl = unMod absMethodDeclM

absMethodDeclM :: P (Mod (MemberDecl Pa))
absMethodDeclM = do
    pos <- getParaPosition
    tps <- lopt typeParams
    rt  <- returnType
    i   <- ident
    fps <- formalParams
    thr <- lopt throws
    semiColon
    return $ \ms ->
        generalize tps $ MethodDecl pos ms tps rt i fps thr (MethodBody pos Nothing)

throws :: P [ExceptionSpec Pa]
throws = tok KW_Throws >> seplist1 exceptionSpec comma

exceptionSpec :: P (ExceptionSpec Pa)
exceptionSpec = setPos ExceptionSpec <*> list modifier <*> refType

-- Formal parameters

formalParams :: P [FormalParam Pa]
formalParams = parens $ do
    fps <- seplist formalParam comma
    if validateFPs fps
     then return fps
     else fail "Only the last formal parameter may be of variable arity"

  where validateFPs :: [FormalParam Pa] -> Bool
        validateFPs [] = True
        validateFPs [_] = True
        validateFPs (FormalParam _ _ _ b _ :xs) = not b && validateFPs xs

formalParam :: P (FormalParam Pa)
formalParam = setPos FormalParam <*>
      list modifier <*>
      ttype <*>
      bopt ellipsis <*>
      varDeclId

ellipsis :: P ()
ellipsis = period >> period >> period

-- Modifiers

modifier :: P (Modifier Pa)
modifier =
        tok KW_Public      >> setPos Public
    <|> tok KW_Protected   >> setPos Protected
    <|> tok KW_Private     >> setPos Private
    <|> tok KW_Abstract    >> setPos Abstract
    <|> tok KW_Static      >> setPos Static
    <|> tok KW_Strictfp    >> setPos StrictFP
    <|> tok KW_Final       >> setPos Final
    <|> tok KW_Native      >> setPos Native
    <|> tok KW_Transient   >> setPos Transient
    <|> tok KW_Volatile    >> setPos Volatile

    <|> tok KW_P_Typemethod  >> setPos Typemethod
    <|> tok KW_P_Notnull     >> setPos Notnull
    <|> tok KW_P_Readonly    >> setPos Readonly
    <|> tok KW_P_Reflexive   >> setPos Reflexive
    <|> tok KW_P_Transitive  >> setPos Transitive
    <|> tok KW_P_Symmetric   >> setPos Symmetric
    <|> tok Op_Query >> setPos Reads   <*> policy
    <|> tok Op_Bang  >> setPos Writes  <*> policy
    <|> tok Op_Plus  >> setPos Opens   <*> lockExp
    <|> tok Op_Minus >> setPos Closes  <*> lockExp
    <|> tok Op_Tilde >> setPos Expects <*> lockExp

----------------------------------------------------------------------------
-- Variable declarations

varDecls :: P (VarDecl Pa) -> P [VarDecl Pa]
varDecls varDeclFun = seplist1 varDeclFun comma

varDecl :: P (VarDecl Pa)
varDecl = setPos VarDecl <*> varDeclId <*> opt (tok Op_Equal >> varInit)

interfaceVarDecl :: P (VarDecl Pa)
interfaceVarDecl = setPos VarDecl <*> varDeclId <*> (Just <$> (tok Op_Equal >> varInit))

varDeclId :: P (VarDeclId Pa)
varDeclId = do
    pos <- getParaPosition
    i  <- ident
    bs <- list arrBrackets
    return $ foldl (\f pos' -> VarDeclArray pos' . f) (VarId pos) bs i

arrBrackets :: P SourcePos
arrBrackets = brackets getParaPosition

localVarDecl :: P ([Modifier Pa], Type Pa, [VarDecl Pa])
localVarDecl = do
    ms  <- list modifier
    typ <- ttype
    vds <- varDecls varDecl
    return (ms, typ, vds)

-- setPos :: (SourcePos -> (ArrayInit Pa -> VarInit Pa))
--          ->  P (ArrayInit Pa -> VarInit Pa) <*> P (ArrayInit Pa)
-- setPos :: (SourcePos -> b) -> P b
-- setPos x = getPosition >>= \pos -> return $ x (parSecToSourcePos pos)

-- InitArray :: SourcePos -> ArrayInit Pa -> VarInit Pa
varInit :: P (VarInit Pa)
varInit =
    try (setPos InitArray <*> arrayInit) <|>
    setPos InitExp <*> exp

arrayInit :: P (ArrayInit Pa)
arrayInit = braces $ do
    pos <- getParaPosition
    vis <- seplist varInit comma
    _ <- opt comma
    return $ ArrayInit pos vis

----------------------------------------------------------------------------
-- Statements

block :: P (Block Pa)
block = braces $ setPos Block <*> list blockStmt

blockStmt :: P (BlockStmt Pa)
blockStmt =
    (try $ do
        pos <- getParaPosition
        ms  <- list modifier
        cd  <- classDeclM
        return $ LocalClass pos (cd ms)) <|>
    (try $ do
        pos <- getParaPosition
        (m,t,vds) <- endSemi localVarDecl
        return $ LocalVars pos m t vds) <|>
    (try $ endSemi $ do
        pos <- getParaPosition
        ms  <- list modifier
        tok KW_P_Lock
        lc  <- ident
        ar  <- lopt arity
        lp  <- opt lockProperties
        return $ LocalLock pos ms lc ar lp) <|>
{-    (try $ endSemi $ do
        ms  <- list modifier
        tok KW_P_Policy
        ln  <- ident
        tok Op_Equal
        pol <- policy
        return $ LocalPolicy ms ln pol) <|> -}
    setPos BlockStmt <*> stmt

stmt :: P (Stmt Pa)
stmt =
    -- ifThen and ifThenElse, with a common prefix
    (do pos <- getParaPosition
        tok KW_If
        e   <- parens exp
        (try $
            do th <- stmtNSI
               tok KW_Else
               el <- stmt
               return $ IfThenElse pos e th el) <|>
           (do th <- stmt
               return $ IfThen     pos e th)  ) <|>
    -- while loops
    (do pos <- getParaPosition
        tok KW_While
        e   <- parens exp
        s   <- stmt
        return $ While pos e s) <|>
    -- basic and enhanced for
    (do pos <- getParaPosition
        tok KW_For
        f <- parens $
            (try $ do
                fi <- opt forInit
                semiColon
                e  <- opt exp
                semiColon
                fu <- opt forUp
                return $ BasicFor pos fi e fu) <|>
            (do ms <- list modifier
                t  <- ttype
                i  <- ident
                colon
                e  <- exp
                return $ EnhancedFor pos ms t i e)
        s <- stmt
        return $ f s) <|>
    -- labeled statements
    (try $ do
        pos <- getParaPosition
        lbl <- ident
        colon
        s   <- stmt
        return $ Labeled pos lbl s) <|>
    -- the rest
    stmtNoTrail

stmtNSI :: P (Stmt Pa)
stmtNSI =
    -- if statements - only full ifThenElse
    (do pos <- getParaPosition
        tok KW_If
        e  <- parens exp
        th <- stmtNSI
        tok KW_Else
        el <- stmtNSI
        return $ IfThenElse pos e th el) <|>
    -- while loops
    (do pos <- getParaPosition
        tok KW_While
        e <- parens exp
        s <- stmtNSI
        return $ While pos e s) <|>
    -- for loops, both basic and enhanced
    (do pos <- getParaPosition
        tok KW_For
        f <- parens $ (try $ do
            fi <- opt forInit
            semiColon
            e  <- opt exp
            semiColon
            fu <- opt forUp
            return $ BasicFor pos fi e fu)
            <|> (do
            pos' <- getParaPosition
            ms <- list modifier
            t  <- ttype
            i  <- ident
            colon
            e  <- exp
            return $ EnhancedFor pos' ms t i e)
        s <- stmtNSI
        return $ f s) <|>
    -- labeled stmts
    (try $ do
        pos <- getParaPosition
        i <- ident
        colon
        s <- stmtNSI
        return $ Labeled pos i s) <|>
    -- the rest
    stmtNoTrail


stmtNoTrail :: P (Stmt Pa)
stmtNoTrail =
    -- empty statement
    setPos (const . Empty) <*> semiColon <|>
    -- inner block
    setPos StmtBlock <*> block <|>
    -- assertions
    (endSemi $ do
        pos <- getParaPosition
        tok KW_Assert
        e   <- exp
        me2 <- opt $ colon >> exp
        return $ Assert pos e me2) <|>
    -- switch stmts
    (do pos <- getParaPosition
        tok KW_Switch
        e  <- parens exp
        sb <- switchBlock
        return $ Switch pos e sb) <|>
    -- do-while loops
    (endSemi $ do
        pos <- getParaPosition
        tok KW_Do
        s <- stmt
        tok KW_While
        e <- parens exp
        return $ Do pos s e) <|>
    -- break
    (endSemi $ do
        pos <- getParaPosition
        tok KW_Break
        mi <- opt ident
        return $ Break pos mi) <|>
    -- continue
    (endSemi $ do
        pos <- getParaPosition
        tok KW_Continue
        mi <- opt ident
        return $ Continue pos mi) <|>
    -- return
    (endSemi $ do
        pos <- getParaPosition
        tok KW_Return
        me <- opt exp
        return $ Return pos me) <|>
    -- synchronized
    (do pos <- getParaPosition
        tok KW_Synchronized
        e <- parens exp
        b <- block
        return $ Synchronized pos e b) <|>
    -- throw
    (endSemi $ do
        pos <- getParaPosition
        tok KW_Throw
        e <- exp
        return $ Throw pos e) <|>
    -- try-catch, both with and without a finally clause
    (do pos <- getParaPosition
        tok KW_Try
        b <- block
        c <- list catch
        mf <- opt $ tok KW_Finally >> block
        -- TODO: here we should check that there exists at
        -- least one catch or finally clause
        return $ Try pos b c mf) <|>
    -- Paragon
    -- opening a lock
    (do pos <- getParaPosition
        tok KW_P_Open
        lc <- lock
        (try block >>= (\bl -> return (OpenBlock pos lc bl)) <|> semiColon >> return (Open pos lc))) <|>
    -- closing a lock
    (do
        pos <- getParaPosition
        tok KW_P_Close
        lc <- lock
        {- (try block >>= (\bl -> return (CloseBlock lc bl)) <|> -}
        semiColon >> return (Close pos lc)) <|>

    -- expressions as stmts
    setPos ExpStmt <*> endSemi stmtExp

-- For loops

forInit :: P (ForInit Pa)
forInit = (try $ do
    pos <- getParaPosition
    (m,t,vds) <- localVarDecl
    return $ ForLocalVars pos m t vds) <|>
    setPos ForInitExps <*> seplist1 stmtExp comma

forUp :: P [Exp Pa]
forUp = seplist1 stmtExp comma

-- Switches

switchBlock :: P [SwitchBlock Pa]
switchBlock = braces $ list switchStmt

switchStmt :: P (SwitchBlock Pa)
switchStmt = setPos SwitchBlock <*> switchLabel <*> list blockStmt

switchLabel :: P (SwitchLabel Pa)
switchLabel = tok KW_Default >> colon >> setPos Default <|>
    (do pos <- getParaPosition
        tok KW_Case
        e <- exp
        colon
        return $ SwitchCase pos e)

-- Try-catch clauses

catch :: P (Catch Pa)
catch = do
    pos <- getParaPosition
    tok KW_Catch
    fp <- parens formalParam
    b  <- block
    return $ Catch pos fp b

----------------------------------------------------------------------------
-- Expressions

stmtExp :: P (Exp Pa)
stmtExp = try preIncDec
    <|> try postIncDec
    <|> try assignment
    -- There are sharing gains to be made by unifying these two
    <|> try instanceCreation
    <|> methodInvocationExp

preIncDec :: P (Exp Pa)
preIncDec = do
    op <- preIncDecOp
    e <- unaryExp
    return $ op e

postIncDec :: P (Exp Pa)
postIncDec = do
    e <- postfixExpNES
    ops <- list1 postfixOp
    return $ foldl (\a s -> s a) e ops

assignment :: P (Exp Pa)
assignment = setPos Assign <*> lhs <*> assignOp <*> assignExp

lhs :: P (Lhs Pa)
lhs = try (setPos FieldLhs <*> fieldAccess)
  <|> try (setPos ArrayLhs <*> arrayAccess)
  <|> setPos NameLhs <*> nameRaw eName

exp :: P (Exp Pa)
exp = assignExp

assignExp :: P (Exp Pa)
assignExp = try assignment <|> condExp

condExp :: P (Exp Pa)
-- condExp = error "phasing out condExp"-- do
condExp = do
    ie <- fixPrecs <$> infixExp -- TODO: precedence resolution
    ces <- list condExpSuffix
    return $ foldl (\a s -> s a) ie ces

condExpSuffix :: P (Exp Pa -> Exp Pa)
condExpSuffix = do
    pos <- getParaPosition
    tok Op_Query
    th <- exp
    colon
    el <- condExp
    return $ \ce -> Cond pos ce th el

infixExp :: P (Exp Pa)
infixExp = do
    ue <- unaryExp
    ies <- list infixExpSuffix
    return $ foldl (\a s -> s a) ue ies

infixExpSuffix :: P (Exp Pa -> Exp Pa)
infixExpSuffix =
    (do pos <- getParaPosition
        op <- infixOp
        e2 <- unaryExp
        return $ \e1 -> BinOp pos e1 op e2) <|>
    (do pos <- getParaPosition
        tok KW_Instanceof
        t  <- refType
        return $ \e1 -> InstanceOf pos e1 t)

unaryExp :: P (Exp Pa)
unaryExp = try preIncDec <|>
    try (do
        op <- prefixOp
        ue <- unaryExp
        return $ op ue) <|>
    try (setPos Cast <*> parens ttype <*> unaryExp) <|>
    postfixExp

postfixExpNES :: P (Exp Pa)
postfixExpNES = -- try postIncDec <|>
    try primary <|>
    setPos ExpName <*> nameRaw eOrLName

postfixExp :: P (Exp Pa)
postfixExp = do
    pe <- postfixExpNES
    ops <- list postfixOp
    return $ foldl (\a s -> s a) pe ops


primary :: P (Exp Pa)
primary = primaryNPS |>> primarySuffix

primaryNPS :: P (Exp Pa)
primaryNPS = try arrayCreation <|> primaryNoNewArrayNPS

--primaryNoNewArray :: P (Exp Pa)
--primaryNoNewArray = startSuff primaryNoNewArrayNPS primarySuffix

primaryNoNewArrayNPS :: P (Exp Pa)
primaryNoNewArrayNPS =
    setPos Lit <*> literal <|>
    setPos (const.This) <*> tok KW_This <|>
    setPos Paren <*> parens exp <|>
    setPos PolicyExp <*> policyExp <|>
    setPos LockExp <*> (tok Op_Query >> lock) <|>
    -- TODO: These two following should probably be merged more
    (try $ do
        rt <- returnType
        pos <- getParaPosition
        mt <- checkClassLit rt
        period >> tok KW_Class
        return $ ClassLit pos mt) <|>
    (try $ do
        pos <- getParaPosition
        n <- nameRaw tName
        period >> tok KW_This
        return $ ThisClass pos n) <|>
    try instanceCreationNPS <|>
    try (setPos MethodInv <*> methodInvocationNPS) <|>
    try (setPos FieldAccess <*> fieldAccessNPS) <|>
    setPos ArrayAccess <*> arrayAccessNPS <|>
    setPos AntiQExp <*>
      javaToken (\t ->
          case t of
            AntiQExpTok s -> Just s
            _ -> Nothing)

checkClassLit :: ReturnType Pa -> P (Maybe (Type Pa))
checkClassLit (LockType _) = fail "Lock is not a class type!"
checkClassLit (VoidType _) = return Nothing
checkClassLit (Type _ t)    = return $ Just t


primarySuffix :: P (Exp Pa -> Exp Pa)
primarySuffix = try instanceCreationSuffix <|>
  (do pos <- getParaPosition
      try ((ArrayAccess pos .) <$> arrayAccessSuffix) <|>
        try ((MethodInv pos .) <$> methodInvocationSuffix) <|>
        (FieldAccess pos .) <$> fieldAccessSuffix)

instanceCreationNPS :: P (Exp Pa)
instanceCreationNPS =
    do tok KW_New
       pos <- getParaPosition
       tas <- lopt typeArgs
       ct  <- classType
       as  <- args
       mcb <- opt classBody
       return $ InstanceCreation pos tas ct as mcb

instanceCreationSuffix :: P (Exp Pa -> Exp Pa)
instanceCreationSuffix =
     do period >> tok KW_New
        pos <- getParaPosition
        tas <- lopt typeArgs
        i   <- ident
        as  <- args
        mcb <- opt classBody
        return $ \p -> QualInstanceCreation pos p tas i as mcb

instanceCreation :: P (Exp Pa)
instanceCreation = {- try instanceCreationNPS <|> -} do
    p <- primaryNPS
    ss <- list primarySuffix
    let icp = foldl (\a s -> s a) p ss
    case icp of
     InstanceCreation     {} -> return icp
     QualInstanceCreation {} -> return icp
     _ -> fail "instanceCreation"

{-
instanceCreation =
    (do tok KW_New
        tas <- lopt typeArgs
        ct  <- classType
        as  <- args
        mcb <- opt classBody
        return $ InstanceCreation tas ct as mcb) <|>
    (do p   <- primary
        period >> tok KW_New
        tas <- lopt typeArgs
        i   <- ident
        as  <- args
        mcb <- opt classBody
        return $ QualInstanceCreation p tas i as mcb)
-}

fieldAccessNPS :: P (FieldAccess Pa)
fieldAccessNPS =
    (do tok KW_Super >> period
        setPos SuperFieldAccess <*> ident) <|>
    (do pos <- getParaPosition
        n <- nameRaw tName
        period >> tok KW_Super >> period
        i <- ident
        return $ ClassFieldAccess pos n i)

fieldAccessSuffix :: P (Exp Pa -> FieldAccess Pa)
fieldAccessSuffix = do
    period
    pos <- getParaPosition
    i <- ident
    return $ \p -> PrimaryFieldAccess pos p i

fieldAccess :: P (FieldAccess Pa)
fieldAccess = {- try fieldAccessNPS <|> -} do
    p <- primaryNPS
    ss <- list primarySuffix
    let fap = foldl (\a s -> s a) p ss
    case fap of
     FieldAccess _ fa -> return fa
     _ -> fail ""

fieldAccessExp :: P (Exp Pa)
fieldAccessExp = setPos FieldAccess <*> fieldAccess

{-
fieldAccess :: P FieldAccess
fieldAccess = try fieldAccessNPS <|> do
    p <- primary
    fs <- fieldAccessSuffix
    return (fs p)
-}

{-
fieldAccess :: P FieldAccess
fieldAccess =
    (do tok KW_Super >> period
        i <- ident
        return $ SuperFieldAccess i) <|>
    (try $ do
        n <- name
        period >> tok KW_Super >> period
        i <- ident
        return $ ClassFieldAccess n i) <|>
    (do p <- primary
        period
        i <- ident
        return $ PrimaryFieldAccess p i)
-}

methodInvocationNPS :: P (MethodInvocation Pa)
methodInvocationNPS =
    (do tok KW_Super >> period
        setPos SuperMethodCall <*> lopt nonWildTypeArgs <*> ident <*> args) <|>
    (do n <- nameRaw ambName
        f <- (do pos <- getParaPosition
                 as <- args
                 return $ \na -> MethodCallOrLockQuery pos (mOrLName $ flattenName na) as) <|>
             (period >> do
                pos <- getParaPosition
                msp <- opt (tok KW_Super >> period)
                rts <- lopt nonWildTypeArgs
                i   <- ident
                as  <- args
                let mc = maybe (TypeMethodCall pos) (const (ClassMethodCall pos)) msp
                return $ \na -> mc (tName $ flattenName na) rts i as)
        return $ f n)

methodInvocationSuffix :: P (Exp Pa -> MethodInvocation Pa)
methodInvocationSuffix = do
        period
        pos <- getParaPosition
        rts <- lopt nonWildTypeArgs
        i   <- ident
        as  <- args
        return $ \p -> PrimaryMethodCall pos p rts i as

methodInvocationExp :: P (Exp Pa)
methodInvocationExp = {- try (MethodInv () <$> methodInvocationNPS) <|> -} do
    p <- primaryNPS
    ss <- list primarySuffix
    let mip = foldl (\a s -> s a) p ss
    case mip of
     MethodInv _ _ -> return mip
     _ -> fail ""

{-
methodInvocation :: P MethodInvocation
methodInvocation =
    (do tok KW_Super >> period
        rts <- lopt nonWildTypeArgs
        i   <- ident
        as  <- args
        return $ SuperMethodCall rts i as) <|>
    (do p <- primary
        period
        rts <- lopt nonWildTypeArgs
        i   <- ident
        as  <- args
        return $ PrimaryMethodCall p rts i as) <|>
    (do n <- name
        f <- (do as <- args
                 return $ \n -> MethodCall n as) <|>
             (period >> do
                msp <- opt (tok KW_Super >> period)
                rts <- lopt nonWildTypeArgs
                i   <- ident
                as  <- args
                let mc = maybe TypeMethodCall (const ClassMethodCall) msp
                return $ \n -> mc n rts i as)
        return $ f n)
-}

args :: P [Argument Pa]
args = parens $ seplist exp comma

-- Arrays

arrayAccessNPS :: P (ArrayIndex Pa)
arrayAccessNPS = do
    n <- nameRaw eName
    e <- brackets exp
    setPos ArrayIndex <*> (setPos ExpName <*> pure n) <*> pure e

arrayAccessSuffix :: P (Exp Pa -> ArrayIndex Pa)
arrayAccessSuffix = do
    e <- brackets exp
    pos <- getParaPosition
    return $ \ref -> ArrayIndex pos ref e

arrayAccess :: P (ArrayIndex Pa)
arrayAccess = {- try arrayAccessNPS <|> -} do
    p <- primaryNoNewArrayNPS
    ss <- list primarySuffix
    let aap = foldl (\a s -> s a) p ss
    case aap of
     ArrayAccess _ ain -> return ain
     _ -> fail ""

{-
arrayAccess :: P (Exp, Exp)
arrayAccess = do
    ref <- arrayRef
    e   <- brackets exp
    return (ref, e)

arrayRef :: P Exp
arrayRef = ExpName <$> name <|> primaryNoNewArray
-}

arrayCreation :: P (Exp Pa)
arrayCreation = do
    tok KW_New
    t <- nonArrayType
    f <- (try $ do
             pos <- getParaPosition
             ds <- list1 (brackets empty >> opt (angles argExp))
             ai <- arrayInit
             return $ \ty -> ArrayCreateInit pos ty ds ai) <|>
         (do pos <- getParaPosition
             des <- list1 $ do
                      e <- brackets exp
                      p <- opt (angles argExp)
                      return (e,p)
             ds <- list (brackets empty >> opt (angles argExp))
             return $ \ty -> ArrayCreate pos ty des ds)
    return $ f t

literal :: P (Literal Pa)
literal =
    javaTokenPos $ \t p ->
      let sp = pos2sourcePos p in
      case t of
        IntTok     i -> Just (Int     sp i)
        LongTok    l -> Just (Word    sp l)
        DoubleTok  d -> Just (Double  sp d)
        FloatTok   f -> Just (Float   sp f)
        CharTok    c -> Just (Char    sp c)
        StringTok  s -> Just (String  sp s)
        BoolTok    b -> Just (Boolean sp b)
        NullTok      -> Just (Null    sp)
        _ -> Nothing

-- Operators

preIncDecOp, prefixOp, postfixOp :: P (Exp Pa -> Exp Pa)
preIncDecOp =
    (tok Op_PPlus  >> setPos PreIncrement ) <|>
    (tok Op_MMinus >> setPos PreDecrement )
prefixOp =
    (tok Op_Bang  >> setPos PreNot      ) <|>
    (tok Op_Tilde >> setPos PreBitCompl ) <|>
    (tok Op_Plus  >> setPos PrePlus     ) <|>
    (tok Op_Minus >> setPos PreMinus    )
postfixOp =
    (tok Op_PPlus  >> setPos PostIncrement ) <|>
    (tok Op_MMinus >> setPos PostDecrement )

assignOp :: P (AssignOp Pa)
assignOp =
    (tok Op_Equal    >> setPos EqualA   ) <|>
    (tok Op_StarE    >> setPos MultA    ) <|>
    (tok Op_SlashE   >> setPos DivA     ) <|>
    (tok Op_PercentE >> setPos RemA     ) <|>
    (tok Op_PlusE    >> setPos AddA     ) <|>
    (tok Op_MinusE   >> setPos SubA     ) <|>
    (tok Op_LShiftE  >> setPos LShiftA  ) <|>
    (tok Op_RShiftE  >> setPos RShiftA  ) <|>
    (tok Op_RRShiftE >> setPos RRShiftA ) <|>
    (tok Op_AndE     >> setPos AndA     ) <|>
    (tok Op_CaretE   >> setPos XorA     ) <|>
    (tok Op_OrE      >> setPos OrA      )

infixOp :: P (Op Pa)
infixOp =
    (tok Op_Star    >> setPos Mult      ) <|>
    (tok Op_Slash   >> setPos Div       ) <|>
    (tok Op_Percent >> setPos Rem       ) <|>
    (tok Op_Plus    >> setPos Add       ) <|>
    (tok Op_Minus   >> setPos Sub       ) <|>
    (tok Op_LShift  >> setPos LShift    ) <|>
    (tok Op_RShift  >> setPos RShift    ) <|>
    (tok Op_RRShift >> setPos RRShift   ) <|>
    (tok Op_LThan   >> setPos LThan     ) <|>
    (tok Op_GThan   >> setPos GThan     ) <|>
    (tok Op_LThanE  >> setPos LThanE    ) <|>
    (tok Op_GThanE  >> setPos GThanE    ) <|>
    (tok Op_Equals  >> setPos Equal     ) <|>
    (tok Op_BangE   >> setPos NotEq     ) <|>
    (tok Op_And     >> setPos And       ) <|>
    (tok Op_Caret   >> setPos Xor       ) <|>
    (tok Op_Or      >> setPos Or        ) <|>
    (tok Op_AAnd    >> setPos CAnd      ) <|>
    (tok Op_OOr     >> setPos COr       )

typeArgInfixOp :: P (Op Pa)
typeArgInfixOp =
    (tok Op_Star >> setPos Mult ) <|>
    (tok Op_Plus >> setPos Add  )


----------------------------------------------------------------------------
-- Types

ttype :: P (Type Pa)
ttype = try (setPos RefType <*> refType) <|> setPos PrimType <*> primType
         <|> setPos AntiQType <*>
               javaToken (\t ->
                   case t of
                     AntiQTypeTok s -> Just  s
                     _              -> Nothing)

primType :: P (PrimType Pa)
primType =
    tok KW_Boolean >> setPos BooleanT  <|>
    tok KW_Byte    >> setPos ByteT     <|>
    tok KW_Short   >> setPos ShortT    <|>
    tok KW_Int     >> setPos IntT      <|>
    tok KW_Long    >> setPos LongT     <|>
    tok KW_Char    >> setPos CharT     <|>
    tok KW_Float   >> setPos FloatT    <|>
    tok KW_Double  >> setPos DoubleT
    -- Paragon
--     <|> tok KW_P_Actor  >> setPos ActorT
     <|> tok KW_P_Policy >> setPos PolicyT


refType :: P (RefType Pa)
refType = checkNoExtraEnd refTypeE

refTypeE :: P (RefType Pa, Int)
refTypeE = {- trace "refTypeE" -} (
    (do typ <- setPos ArrayType <*>
               (setPos PrimType <*> primType) <*>
               list1 arrPols
        return (typ, 0))
    <|>
    (do (ct, e) <- classTypeE
        baseType <- setPos ClassRefType <*> pure ct
        if (e == 0)
         then do
          -- TODO: Correct pos?
           mps <- list arrPols
           case mps of
             [] -> return (baseType, e)
             _  -> do typ <- setPos ArrayType <*>
                             (setPos RefType <*> pure baseType) <*> pure mps
                      return (typ, 0)
         else return (baseType, e)
    ) <?> "refType")

arrPols :: P (Maybe (Policy Pa))
arrPols = do
  _ <- arrBrackets
  opt $ angles argExp
--  ExpName () <$> angles (nameRaw eName)
--      <|> PolicyExp () <$> policyExp

nonArrayType :: P (Type Pa)
nonArrayType = setPos PrimType <*> primType <|>
    setPos RefType <*> (setPos ClassRefType <*> classType)


classType :: P (ClassType Pa)
classType = checkNoExtraEnd classTypeE

classTypeE :: P (ClassType Pa, Int)
classTypeE = {- trace "classTypeE" $ -} do
  n <- nameRaw tName
  mtase <- opt typeArgsE
  {- trace ("mtase: " ++ show mtase) $ -}
  clt <- setPos ClassType
  case mtase of
    Just (tas, e) -> return (clt n tas, e)
    Nothing       -> return (clt n [] , 0)

returnType :: P (ReturnType Pa)
returnType = tok KW_Void   >> setPos VoidType <|>
             tok KW_P_Lock >> setPos LockType <|>
             setPos Type <*> ttype <?> "returnType"

classTypeList :: P [ClassType Pa]
classTypeList = seplist1 classType comma

----------------------------------------------------------------------------
-- Type parameters and arguments

typeParams :: P [TypeParam Pa]
typeParams = angles $ seplist1 typeParam comma

typeParam :: P (TypeParam Pa)
typeParam =
  (do tok KW_P_Actor >> setPos ActorParam <*> refType <*> ident) <|>
  (do tok KW_P_Policy >> setPos PolicyParam <*> ident) <|>
  (do tok KW_P_Lock >> arrBrackets >> setPos LockStateParam <*> ident) <|>
  (do setPos TypeParam <*> ident <*> lopt bounds)

bounds :: P [RefType Pa]
bounds = tok KW_Extends >> seplist1 refType (tok Op_And)

checkNoExtraEnd :: P (a, Int) -> P a
checkNoExtraEnd pai = do
  (a, e) <- pai
  unless (e == 0) $ fail "unexpected >"
  return a

typeArgs :: P [TypeArgument Pa]
typeArgs = {- trace "typeArgs" $ -} checkNoExtraEnd typeArgsE

typeArgsE :: P ([TypeArgument Pa], Int)
typeArgsE = {- trace "typeArgsE" $ -}
    do tok Op_LThan {- < -}
       (as, extra) <- typeArgsSuffix
       return (as, extra-1)

typeArgsSuffix :: P ([TypeArgument Pa], Int)
typeArgsSuffix = {- trace "typeArgsSuffix" $ -}
  (do tok Op_Query
      wcArg <- setPos Wildcard <*> opt wildcardBound
      (rest, e) <- typeArgsEnd 0
      return (wcArg:rest, e)) <|>

  (do lArg <- setPos ActualArg <*> parens (setPos ActualLockState <*> seplist1 lock comma)
      (rest, e) <- typeArgsEnd 0
      return (lArg:rest, e)) <|>

  (try $ do (rt, er)  <- refTypeE
            (rest, e) <- typeArgsEnd er
            tArg <- case nameOfRefType rt of
                      Just n -> setPos ActualName <*> pure (ambName (flattenName n)) -- keep as ambiguous
                      _ -> setPos ActualType <*> pure rt
            actArg <- setPos ActualArg <*> pure tArg
            return (actArg:rest, e)) <|>

  (do eArg <- setPos ActualArg <*> (setPos ActualExp <*> argExp)
      (rest, e) <- typeArgsEnd 0
      return (eArg:rest, e))

      where nameOfRefType :: RefType Pa -> Maybe (Name Pa)
            nameOfRefType (ClassRefType _ (ClassType _ n tas)) =
                if null tas then Just n else Nothing
            nameOfRefType _ = Nothing

typeArgsEnd :: Int -> P ([TypeArgument Pa], Int) -- Int for the number of "extra" >
typeArgsEnd n | n > 0 = {- trace ("typeArgsEnd-1: " ++ show n) $ -} return ([], n)
typeArgsEnd _ = {- trace ("typeArgsEnd-2: ") $ -}
    (tok Op_GThan   {- >   -} >> return ([], 1)) <|>
    (tok Op_RShift  {- >>  -} >> return ([], 2)) <|>
    (tok Op_RRShift {- >>> -} >> return ([], 3)) <|>
    (tok Comma >> typeArgsSuffix)

argExp :: P (Exp Pa)
argExp = do
  e1 <- argExp1
  fe <- argExpSuffix
  return $ fe e1

argExp1 :: P (Exp Pa)
argExp1 = setPos PolicyExp <*> policyExp
          <|> try methodInvocationExp
          <|> try fieldAccessExp
          <|> setPos ExpName <*> nameRaw eName

-- ****************

argExpSuffix :: P (Exp Pa -> Exp Pa)
argExpSuffix =
    (do op <- typeArgInfixOp
        e2 <- argExp
        binop <- setPos BinOp -- TODO: Does this make sense?
        return $ \e1 -> binop e1 op e2) <|> return id

wildcardBound :: P (WildcardBound Pa)
wildcardBound =
      tok KW_Extends >> setPos ExtendsBound <*> refType
  <|> tok KW_Super >> setPos SuperBound <*> refType

nonWildTypeArgs :: P [NonWildTypeArgument Pa]
nonWildTypeArgs = typeArgs >>= mapM checkNonWild
  where checkNonWild (ActualArg _ arg) = return arg
        checkNonWild _ = fail "Use of wildcard in non-wild context"


--nonWildTypeArgs :: P [NonWildTypeArgument ()]
--nonWildTypeArgs = angles $ seplist nonWildTypeArg (tok Comma)

----------------------------------------------------------------------------
-- Names

nameRaw :: ([Ident Pa] -> Name Pa) -> P (Name Pa)
nameRaw nf =
    nf <$> seplist1 ident period <|>
        javaTokenPos (\t p -> case t of
          AntiQNameTok s -> Just $ AntiQName (pos2sourcePos p) s
          _ -> Nothing)

name :: P (Name Pa)
name = nameRaw ambName

ident :: P (Ident Pa)
ident = javaTokenPos $ \t p -> case t of
    IdentTok s -> Just $ Ident (pos2sourcePos p) (B.pack s)
    AntiQIdentTok s -> Just $ AntiQIdent (pos2sourcePos p) s
    _ -> Nothing

----------------------------------------------------------------------------
-- Policies

policy :: P (Policy Pa)
policy = postfixExpNES -- Policy <$> policyLit <|> PolicyRef <$> (tok Op_Tilde >> name)

policyExp :: P (PolicyExp Pa)
policyExp =
  try (setPos PolicyLit <*> (braces $ seplist clause semiColon)) <|>
  setPos PolicyLit <*> (braces colon >> return []) <|>
  tok KW_P_Policyof >> parens (setPos PolicyOf <*> ident <|>
                               (do pol <- setPos PolicyThis
                                   const pol <$> tok KW_This))

clause :: P (Clause Pa)
clause = do
    pos <- getParaPosition
    vs <- lopt $ parens $ seplist cvardecl comma
    ch <- chead
    ats <- lopt $ colon >> seplist atom comma
    let avdis = map (\(ClauseVarDecl _ _ i) -> i) $
                 case ch of
                   ClauseDeclHead _ cvd -> cvd:vs
                   _ -> vs
        ch' = genActorVars avdis ch
        ats' = genActorVars avdis ats
    return $ Clause pos vs ch' ats'


cvardecl :: P (ClauseVarDecl Pa)
cvardecl = setPos ClauseVarDecl <*> refType <*> ident

chead :: P (ClauseHead Pa)
chead = try (setPos ClauseDeclHead <*> cvardecl) <|>
        setPos ClauseVarHead <*> actor

lclause :: P (LClause Pa)
lclause = do
    pos <- getParaPosition
    qs <- lopt $ parens $ seplist cvardecl comma
    mh <- lclauseHead
    as <- lopt $ colon >> seplist atom comma
    let avdis = map (\(ClauseVarDecl _ _ i) -> i) qs
        as'   = genActorVars avdis as

    case mh of
      Just h -> do
              let [h'] = genActorVars avdis [h]
              return $ LClause pos qs h' as'
      Nothing -> return $ ConstraintClause pos qs as'


lclauseHead :: P (Maybe (Atom Pa))
lclauseHead =
  (tok Op_Minus >> return Nothing) <|>
   (Just <$> atom)

atom :: P (Atom Pa)
atom = do
    setPos Atom <*> nameRaw lName
                <*> (lopt $ parens $ seplist actor comma)

-- We parse everything as actorNames, and post-process
-- them into Vars
actor :: P (Actor Pa)
actor = setPos Actor <*> actorName -- <|>
--        setPos Var <*> actorVar -- (tok Op_Query >> ident)

actorName :: P (ActorName Pa)
actorName = setPos ActorName <*> nameRaw eName

--actorVar :: P (Ident Pa)
--actorVar = javaTokenPos $ \t p -> case t of
--    VarActorTok s -> Just $ Ident (pos2sourcePos p) (B.pack s)
--    _ -> Nothing

lock :: P (Lock Pa)
lock = do
    setPos Lock <*> nameRaw lName
                <*> (lopt $ parens $ seplist actorName comma)

lockProperties :: P (LockProperties Pa)
lockProperties = do braces $ setPos LockProperties
                             <*> optendseplist lclause semiColon

lockExp :: P [Lock Pa]
lockExp = parens (seplist1 lock comma)
      <|> return <$> lock


------------------------------------------------------------

empty :: P ()
empty = return ()

opt :: P a -> P (Maybe a)
opt pa = --optionMaybe
  try (Just <$> pa) <|> return Nothing

bopt :: P a -> P Bool
bopt p = opt p >>= \ma -> return $ isJust ma

lopt :: P [a] -> P [a]
lopt p = do mas <- opt p
            case mas of
             Nothing -> return []
             Just as -> return as

list :: P a -> P [a]
list = option [] . list1

list1 :: P a -> P [a]
list1 = many1

seplist :: P a -> P sep -> P [a]
--seplist = sepBy
seplist p sep = option [] $ seplist1 p sep

seplist1 :: P a -> P sep -> P [a]
--seplist1 = sepBy1
seplist1 p sep =
    p >>= \a ->
        try (do _ <- sep
                as <- seplist1 p sep
                return (a:as))
        <|> return [a]

optendseplist :: P a -> P sep -> P [a]
optendseplist p sep = seplist p sep `optend` sep

optend :: P a -> P end -> P a
optend p end = do
  x <- p
  _ <- opt end
  return x

startSuff, (|>>) :: P a -> P (a -> a) -> P a
startSuff start suffix = do
    x <- start
    ss <- list suffix
    return $ foldl (\a s -> s a) x ss

(|>>) = startSuff

------------------------------------------------------------

javaToken :: (Token -> Maybe a) -> P a
javaToken test = token showT posT testT
  where showT (L _ t) = show t
        posT  (L p _) = extractPSPos $ pos2sourcePos p
        testT (L _ t) = test t

javaTokenPos :: (Token -> (Int,Int) -> Maybe a) -> P a
javaTokenPos test = token showT posT testT
  where showT (L _ t) = show t
        posT  (L p _) = extractPSPos $ pos2sourcePos p
        testT (L p t) = test t p

tok, matchToken :: Token -> P ()
tok = matchToken
matchToken t = javaToken (\r -> if r == t then Just () else Nothing)

-- | Convert a token (line, column) to a source position representation.
-- TODO would be nice to add filename here?
pos2sourcePos :: (Int, Int) -> SourcePos
pos2sourcePos (l,c) = newPos "" l c

type Mod a = [Modifier Pa] -> a

parens, braces, brackets, angles :: P a -> P a
parens   = between (tok OpenParen)  (tok CloseParen)
braces   = between (tok OpenCurly)  (tok CloseCurly)
brackets = between (tok OpenSquare) (tok CloseSquare)
angles   = between (tok Op_LThan)   (tok Op_GThan)

endSemi :: P a -> P a
endSemi p = p >>= \a -> semiColon >> return a

comma, colon, semiColon, period :: P ()
comma     = tok Comma
colon     = tok Op_Colon
semiColon = tok SemiColon
period    = tok Period

------------------------------------------------------------

checkConstrs :: ClassDecl Pa -> P ()
checkConstrs (ClassDecl _ _ i _ _ _ cb) = do
    let errs = [ ci | ConstructorDecl _ _ _ ci _ _ _ <- universeBi cb, ci /= i ]
    if null errs then return ()
     else fail $ "Declaration of class " -- ++ prettyPrint i
                  ++ " cannot contain constructor with name "
                --  ++ prettyPrint (head errs)
checkConstrs _ = panic (parserModule ++ ".checkConstrs")
                 "Checking constructors of Enum decl"

-----------------------------------------------------
-- Making the meaning of a name explicit

ambName :: [Ident Pa] -> Name Pa
ambName = paMkUniformName const AmbName

-- A package name can only have a package name prefix
pName :: [Ident Pa] -> Name Pa
pName = paMkUniformName const PName

-- A package-or-type name has a package-or-type prefix
pOrTName :: [Ident Pa] -> Name Pa
pOrTName = paMkUniformName const POrTName

-- A type name has a package-or-type prefix
tName :: [Ident Pa] -> Name Pa
tName = paMkName const TName POrTName

-- paMkName is a specific case of mkName from syntaxTTG
paMkName :: (XName Pa -> XName Pa -> XName Pa) -> NameType
       -> NameType -> [Ident Pa] -> Name Pa
paMkName f nt ntPre ids = mkName' (reverse ids)
    where mkName' [] = panic (syntaxModule ++ ".mkName")
                             "Empty list of idents"
          mkName' [i] = Name (paAnnId i) nt Nothing i
          mkName' (i:is) =
              let pre = paMkUniformName f ntPre (reverse is)
                  a = f (paAnn pre) (paAnnId i)
              in Name a nt (Just pre) i

-- Specific case of mkUniformName from syntaxTTG
paMkUniformName :: (XName Pa -> XName Pa -> XName Pa)
              -> NameType -> [Ident Pa] -> Name Pa
paMkUniformName f nt ids = mkName' (reverse ids)
    where mkName' [] = panic (syntaxModule ++ ".mkUniformName")
                             "Empty list of idents"
          mkName' [i] = Name (paAnnId i) nt Nothing i
          mkName' (i:is) =
              let pre = mkName' is
                  a = f (paAnn pre) (paAnnId i)
              in Name a nt (Just pre) i

-- specific cases of ann from syntaxTTG
paAnnId :: Ident Pa -> SourcePos
paAnnId (Ident sp _)    = sp
paAnnId _ = error "pattern match paAnnId in parser"

paAnn :: Name Pa -> SourcePos
paAnn (Name sp _ _ _) = sp
paAnn _ = error "pattern match paAnn in parser"

-- Names with ambiguous prefixes
eName, lName, eOrLName, mOrLName :: [Ident Pa] -> Name Pa
eName     = paMkName const EName AmbName
lName     = paMkName const LName AmbName
eOrLName  = paMkName const EOrLName AmbName
mOrLName  = paMkName const MOrLName AmbName

-----------------------------------------------------


-- Generalization is only needed for parameters of
-- kind Type, since these are representated by a
-- special contructor TypeVariable.
-- LockStateVar is handled by the parser, NO LONGER
-- and actors and policies are parsed as ExpName.
generalize :: Data a => [TypeParam Pa] -> a -> a
generalize pars = transformBi gen
                  . transformBi genA
                  . transformBi genP
                  . transformBi genL
    where gen :: RefType Pa -> RefType Pa
          gen (ClassRefType pos (ClassType _ (Name _ TName Nothing i) []))
              | i `elem` parIs = TypeVariable pos i
          gen rt = rt

          genA :: ActorName Pa -> ActorName Pa
          genA (ActorName pos (Name _ EName Nothing i))
               | Just rt <- lookup i actIs = ActorTypeVar pos rt i
          genA a = a

          genP :: Exp Pa -> Exp Pa
          genP (ExpName pos (Name _ EName Nothing i))
              | i `elem` polIs = PolicyExp pos (PolicyTypeVar pos i)
          genP e = e

          genL :: Lock Pa -> Lock Pa
          genL (Lock pos (Name _ LName Nothing i) [])
              | i `elem` locIs = LockVar pos i
          genL l = l

          parIs = [ i | TypeParam _ i _ <- pars ]
          locIs = [ i | LockStateParam _ i <- pars ]
          polIs = [ i | PolicyParam _ i <- pars ]
          actIs = [ (i, rt) | ActorParam _ rt i <- pars ]


-- Generalization of variables in a policy literal
genActorVars :: Data x => [Ident Pa] -> x -> x
genActorVars is = transformBi gen
  where --gen :: Actor a -> Actor a
        gen (Actor _ (ActorName _ (Name _ _ Nothing i)))
            | i `elem` is = Var (paAnnId i) i
        gen ac = ac

-- TODO: Temporary ann definiton. The real one is probably form Annotated.hs
--ann = undefined

--------------------------------------------------------------
-- Resolving precedences
-- () instance created in decorations to fix this function
builtInPrecs :: [(Op PCS, Int)]
builtInPrecs =
    map (,9) [Mult   (), Div    (), Rem     ()            ] ++
    map (,8) [Add    (), Sub    ()                         ] ++
    map (,7) [LShift (), RShift (), RRShift ()            ] ++
    map (,6) [LThan  (), GThan  (), LThanE  (), GThanE ()] ++
    map (,5) [Equal  (), NotEq  ()                         ] ++
    [(And  (), 4) ,
     (Or   (), 3),
     (Xor  (), 2),
     (CAnd (), 1),
     (COr  (), 0)]

instanceOfPrec :: Int
instanceOfPrec = 6 -- same as comparison ops

-- TODO: remove, using PCS is a hack
dropData :: Op a -> Op PCS
dropData (Mult _)    = Mult    ()
dropData (Div _)     = Div     ()
dropData (Rem _)     = Rem     ()
dropData (Add _)     = Add     ()
dropData (Sub _)     = Sub     ()
dropData (LShift _)  = LShift  ()
dropData (RShift _)  = RShift  ()
dropData (RRShift _) = RRShift ()
dropData (LThan _)   = LThan   ()
dropData (GThan _)   = GThan   ()
dropData (LThanE _)  = LThanE  ()
dropData (GThanE _)  = GThanE  ()
dropData (Equal _)   = Equal   ()
dropData (NotEq _)   = NotEq   ()
dropData (And _)     = And     ()
dropData (Or _)      = Or      ()
dropData (Xor _)     = Xor     ()
dropData (CAnd _)    = CAnd    ()
dropData (COr _)     = COr     ()

-- deriving instance Eq (Op ())
-- -- TODO: Fix positions?
fixPrecs :: Exp Pa -> Exp Pa
fixPrecs (BinOp pos a op2 z) =
    let e = fixPrecs a -- recursively fix left subtree
        getPrec op = fromJust $ lookup op builtInPrecs
        fixup p1 p2 y pre =
            if p1 >= p2
             then BinOp pos e op2 z -- already right order
             else pre (fixPrecs $ BinOp pos y op2 z)
    in case e of
         BinOp pos' x op1 y   -> fixup (getPrec . dropData $ op1)  (getPrec . dropData $ op2) y (BinOp pos' x op1)
         InstanceOf pos' y rt -> fixup instanceOfPrec (getPrec . dropData $ op2) y (flip (InstanceOf pos') rt)
         _ -> BinOp pos e op2 z
fixPrecs e = e
