{-# LANGUAGE CPP, PatternGuards, TupleSections #-}
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
import Language.Java.Paragon.Decorations as D
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

----------------------------------------------------------------------------
-- Top-level parsing

parser :: P a -> String -> Either ParseError a
parser p = runParser p () "" . lexer

-- | Convert a parsec SourcePos to a paragon SourcePos
getParaPosition :: P Pa
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
    return $ PPackageDecl pos n

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
            = PSingleTypeImport pos $ flattenRealName tName n
        mkImportDecl pos False True  n
            = PTypeImportOnDemand pos $ flattenRealName pOrTName n
        mkImportDecl pos True  True  n
            = PStaticImportOnDemand pos $ flattenRealName tName n
        mkImportDecl pos True  False n@Name{} =
            let is = flattenName n
            in case reverse is of
                 [] -> panic (parserModule ++ ".importDecl") "Empty name"
                 (lastI:initN) ->
                     PSingleStaticImport pos
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
        return $ \ms -> PClassTypeDecl pos (cdecl ms)) <|>
    (do pos <- getParaPosition
        idecl <- interfaceDeclM
        return $ \ms -> PInterfaceTypeDecl pos (idecl ms))

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
        generalize tps $ PClassDecl pos ms i tps (fmap head mex) imp bod

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
    return $ \ms -> PEnumDecl pos ms i imp bod

classBody :: P (ClassBody Pa)
classBody = setPos PClassBody <*> braces classBodyDecls

enumBody :: P (EnumBody Pa)
enumBody = braces $ do
    pos <- getParaPosition
    ecs <- seplist enumConst comma
    optional comma
    eds <- lopt enumBodyDecls
    return $ PEnumBody pos ecs eds

enumConst :: P (EnumConstant Pa)
enumConst = setPos PEnumConstant
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
        generalize tps $ PInterfaceDecl pos ms i tps exs bod

interfaceBody :: P (InterfaceBody Pa)
interfaceBody = do pos <- getParaPosition
                   PInterfaceBody pos . catMaybes <$>
                      braces (list interfaceBodyDecl)

-- Declarations

classBodyDecl :: P (Decl Pa)
classBodyDecl =
    (try $ setPos PInitDecl
                <*> bopt (tok KW_Static)
                <*> block) <|>
    (do pos <- getParaPosition
        ms  <- list modifier
        dec <- memberDeclM
        return $ PMemberDecl pos (dec ms))

-- Not used internally:
-- | Top-level parser for member declarations
memberDecl :: P (MemberDecl Pa)
memberDecl = unMod memberDeclM

memberDeclM :: P (Mod (MemberDecl Pa))
memberDeclM = {- trace "memberDeclM" $ -}
    (try $ do
        pos <- getParaPosition
        cd  <- classDeclM
        return $ \ms -> PMemberClassDecl pos (cd ms)) <|>
    (try $ do
        pos <- getParaPosition
        idecl  <- interfaceDeclM
        return $ \ms -> PMemberInterfaceDecl pos (idecl ms)) <|>
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
    return $ \ms -> PFieldDecl pos ms typ vds

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
        generalize tps $ PMethodDecl pos ms tps rt i fps thr bod

methodBody :: P (MethodBody Pa)
methodBody = setPos PMethodBody <*>
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
        generalize tps $ PConstructorDecl pos ms tps i fps thr bod

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
    return $ \ms -> PLockDecl pos ms lc ar lp

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
constrBody = braces $ do setPos PConstructorBody <*>
                           opt (try explConstrInv) <*>
                           list blockStmt

explConstrInv :: P (ExplConstrInv Pa)
explConstrInv = endSemi $
    (try $ do
        pos <- getParaPosition
        tas <- lopt nonWildTypeArgs
        tok KW_This
        as  <- args
        return $ PThisInvoke pos tas as) <|>
    (try $ do
        pos <- getParaPosition
        tas <- lopt nonWildTypeArgs
        tok KW_Super
        as  <- args
        return $ PSuperInvoke pos tas as) <|>
    (do pos <- getParaPosition
        pri <- primary
        period
        tas <- lopt nonWildTypeArgs
        tok KW_Super
        as  <- args
        return $ PPrimarySuperInvoke pos pri tas as)

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
        return $ \ms -> PMemberClassDecl pos (cd ms)) <|>
    (do pos <- getParaPosition
        idecl  <- interfaceDeclM
        return $ \ms -> PMemberInterfaceDecl pos (idecl ms)) <|>
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
        generalize tps $ PMethodDecl pos ms tps rt i fps thr (PMethodBody pos Nothing)

throws :: P [ExceptionSpec Pa]
throws = tok KW_Throws >> seplist1 exceptionSpec comma

exceptionSpec :: P (ExceptionSpec Pa)
exceptionSpec = setPos PExceptionSpec <*> list modifier <*> refType

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
        validateFPs (PFormalParam _ _ _ b _ :xs) = not b && validateFPs xs

formalParam :: P (FormalParam Pa)
formalParam = setPos PFormalParam <*>
      list modifier <*>
      ttype <*>
      bopt ellipsis <*>
      varDeclId

ellipsis :: P ()
ellipsis = period >> period >> period

-- Modifiers

modifier :: P (Modifier Pa)
modifier =
        tok KW_Public      >> setPos PPublic
    <|> tok KW_Protected   >> setPos PProtected
    <|> tok KW_Private     >> setPos PPrivate
    <|> tok KW_Abstract    >> setPos PAbstract
    <|> tok KW_Static      >> setPos PStatic
    <|> tok KW_Strictfp    >> setPos PStrictFP
    <|> tok KW_Final       >> setPos PFinal
    <|> tok KW_Native      >> setPos PNative
    <|> tok KW_Transient   >> setPos PTransient
    <|> tok KW_Volatile    >> setPos PVolatile

    <|> tok KW_P_Typemethod  >> setPos PTypemethod
    <|> tok KW_P_Notnull     >> setPos PNotnull
    <|> tok KW_P_Readonly    >> setPos PReadonly
    <|> tok KW_P_Reflexive   >> setPos PReflexive
    <|> tok KW_P_Transitive  >> setPos PTransitive
    <|> tok KW_P_Symmetric   >> setPos PSymmetric
    <|> tok Op_Query >> setPos PReads   <*> policy
    <|> tok Op_Bang  >> setPos PWrites  <*> policy
    <|> tok Op_Plus  >> setPos POpens   <*> lockExp
    <|> tok Op_Minus >> setPos PCloses  <*> lockExp
    <|> tok Op_Tilde >> setPos PExpects <*> lockExp

----------------------------------------------------------------------------
-- Variable declarations

varDecls :: P (VarDecl Pa) -> P [VarDecl Pa]
varDecls varDeclFun = seplist1 varDeclFun comma

varDecl :: P (VarDecl Pa)
varDecl = setPos PVarDecl <*> varDeclId <*> opt (tok Op_Equal >> varInit)

interfaceVarDecl :: P (VarDecl Pa)
interfaceVarDecl = setPos PVarDecl <*> varDeclId <*> (Just <$> (tok Op_Equal >> varInit))

varDeclId :: P (VarDeclId Pa)
varDeclId = do
    pos <- getParaPosition
    i  <- ident
    bs <- list arrBrackets
    return $ foldl (\f pos' -> PVarDeclArray pos' . f) (PVarId pos) bs i

arrBrackets :: P Pa
arrBrackets = brackets getParaPosition

localVarDecl :: P ([Modifier Pa], Type Pa, [VarDecl Pa])
localVarDecl = do
    ms  <- list modifier
    typ <- ttype
    vds <- varDecls varDecl
    return (ms, typ, vds)

varInit :: P (VarInit Pa)
varInit =
    try (setPos PInitArray <*> arrayInit) <|>
    setPos PInitExp <*> exp

arrayInit :: P (ArrayInit Pa)
arrayInit = braces $ do
    pos <- getParaPosition
    vis <- seplist varInit comma
    _ <- opt comma
    return $ PArrayInit pos vis

----------------------------------------------------------------------------
-- Statements

block :: P (Block Pa)
block = braces $ setPos PBlock <*> list blockStmt

blockStmt :: P (BlockStmt Pa)
blockStmt =
    (try $ do
        pos <- getParaPosition
        ms  <- list modifier
        cd  <- classDeclM
        return $ PLocalClass pos (cd ms)) <|>
    (try $ do
        pos <- getParaPosition
        (m,t,vds) <- endSemi localVarDecl
        return $ PLocalVars pos m t vds) <|>
    (try $ endSemi $ do
        pos <- getParaPosition
        ms  <- list modifier
        tok KW_P_Lock
        lc  <- ident
        ar  <- lopt arity
        lp  <- opt lockProperties
        return $ PLocalLock pos ms lc ar lp) <|>
{-    (try $ endSemi $ do
        ms  <- list modifier
        tok KW_P_Policy
        ln  <- ident
        tok Op_Equal
        pol <- policy
        return $ LocalPolicy ms ln pol) <|> -}
    setPos PBlockStmt <*> stmt

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
               return $ PIfThenElse pos e th el) <|>
           (do th <- stmt
               return $ PIfThen     pos e th)  ) <|>
    -- while loops
    (do pos <- getParaPosition
        tok KW_While
        e   <- parens exp
        s   <- stmt
        return $ PWhile pos e s) <|>
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
                return $ PBasicFor pos fi e fu) <|>
            (do ms <- list modifier
                t  <- ttype
                i  <- ident
                colon
                e  <- exp
                return $ PEnhancedFor pos ms t i e)
        s <- stmt
        return $ f s) <|>
    -- labeled statements
    (try $ do
        pos <- getParaPosition
        lbl <- ident
        colon
        s   <- stmt
        return $ PLabeled pos lbl s) <|>
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
        return $ PIfThenElse pos e th el) <|>
    -- while loops
    (do pos <- getParaPosition
        tok KW_While
        e <- parens exp
        s <- stmtNSI
        return $ PWhile pos e s) <|>
    -- for loops, both basic and enhanced
    (do pos <- getParaPosition
        tok KW_For
        f <- parens $ (try $ do
            fi <- opt forInit
            semiColon
            e  <- opt exp
            semiColon
            fu <- opt forUp
            return $ PBasicFor pos fi e fu)
            <|> (do
            pos' <- getParaPosition
            ms <- list modifier
            t  <- ttype
            i  <- ident
            colon
            e  <- exp
            return $ PEnhancedFor pos' ms t i e)
        s <- stmtNSI
        return $ f s) <|>
    -- labeled stmts
    (try $ do
        pos <- getParaPosition
        i <- ident
        colon
        s <- stmtNSI
        return $ PLabeled pos i s) <|>
    -- the rest
    stmtNoTrail


stmtNoTrail :: P (Stmt Pa)
stmtNoTrail =
    -- empty statement
    setPos (const . PEmpty) <*> semiColon <|>
    -- inner block
    setPos PStmtBlock <*> block <|>
    -- assertions
    (endSemi $ do
        pos <- getParaPosition
        tok KW_Assert
        e   <- exp
        me2 <- opt $ colon >> exp
        return $ PAssert pos e me2) <|>
    -- switch stmts
    (do pos <- getParaPosition
        tok KW_Switch
        e  <- parens exp
        sb <- switchBlock
        return $ PSwitch pos e sb) <|>
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
        return $ PBreak pos mi) <|>
    -- continue
    (endSemi $ do
        pos <- getParaPosition
        tok KW_Continue
        mi <- opt ident
        return $ PContinue pos mi) <|>
    -- return
    (endSemi $ do
        pos <- getParaPosition
        tok KW_Return
        me <- opt exp
        return $ PReturn pos me) <|>
    -- synchronized
    (do pos <- getParaPosition
        tok KW_Synchronized
        e <- parens exp
        b <- block
        return $ PSynchronized pos e b) <|>
    -- throw
    (endSemi $ do
        pos <- getParaPosition
        tok KW_Throw
        e <- exp
        return $ PThrow pos e) <|>
    -- try-catch, both with and without a finally clause
    (do pos <- getParaPosition
        tok KW_Try
        b <- block
        c <- list catch
        mf <- opt $ tok KW_Finally >> block
        -- TODO: here we should check that there exists at
        -- least one catch or finally clause
        return $ PTry pos b c mf) <|>
    -- Paragon
    -- opening a lock
    (do pos <- getParaPosition
        tok KW_P_Open
        lc <- lock
        (try block >>= (\bl -> return (POpenBlock pos lc bl)) <|> semiColon >> return (POpen pos lc))) <|>
    -- closing a lock
    (do
        pos <- getParaPosition
        tok KW_P_Close
        lc <- lock
        {- (try block >>= (\bl -> return (CloseBlock lc bl)) <|> -}
        semiColon >> return (PClose pos lc)) <|>

    -- expressions as stmts
    setPos PExpStmt <*> endSemi stmtExp

-- For loops

forInit :: P (ForInit Pa)
forInit = (try $ do
    pos <- getParaPosition
    (m,t,vds) <- localVarDecl
    return $ PForLocalVars pos m t vds) <|>
    setPos PForInitExps <*> seplist1 stmtExp comma

forUp :: P [Exp Pa]
forUp = seplist1 stmtExp comma

-- Switches

switchBlock :: P [SwitchBlock Pa]
switchBlock = braces $ list switchStmt

switchStmt :: P (SwitchBlock Pa)
switchStmt = setPos PSwitchBlock <*> switchLabel <*> list blockStmt

switchLabel :: P (SwitchLabel Pa)
switchLabel = tok KW_Default >> colon >> setPos PDefault <|>
    (do pos <- getParaPosition
        tok KW_Case
        e <- exp
        colon
        return $ PSwitchCase pos e)

-- Try-catch clauses

catch :: P (Catch Pa)
catch = do
    pos <- getParaPosition
    tok KW_Catch
    fp <- parens formalParam
    b  <- block
    return $ PCatch pos fp b

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
assignment = setPos PAssign <*> lhs <*> assignOp <*> assignExp

lhs :: P (Lhs Pa)
lhs = try (setPos PFieldLhs <*> fieldAccess)
  <|> try (setPos PArrayLhs <*> arrayAccess)
  <|> setPos PNameLhs <*> nameRaw eName

exp :: P (Exp Pa)
exp = assignExp

assignExp :: P (Exp Pa)
assignExp = try assignment <|> condExp

condExp :: P (Exp Pa)
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
    return $ \ce -> PCond pos ce th el

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
        return $ \e1 -> PBinOp pos e1 op e2) <|>
    (do pos <- getParaPosition
        tok KW_Instanceof
        t  <- refType
        return $ \e1 -> PInstanceOf pos e1 t)

unaryExp :: P (Exp Pa)
unaryExp = try preIncDec <|>
    try (do
        op <- prefixOp
        ue <- unaryExp
        return $ op ue) <|>
    try (setPos PCast <*> parens ttype <*> unaryExp) <|>
    postfixExp

postfixExpNES :: P (Exp Pa)
postfixExpNES = -- try postIncDec <|>
    try primary <|>
    setPos PExpName <*> nameRaw eOrLName

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
    setPos PLit <*> literal <|>
    setPos (const.PThis) <*> tok KW_This <|>
    setPos PParen <*> parens exp <|>
    setPos PPolicyExp <*> policyExp <|>
    setPos PLockExp <*> (tok Op_Query >> lock) <|>
    -- TODO: These two following should probably be merged more
    (try $ do
        rt <- returnType
        pos <- getParaPosition
        mt <- checkClassLit rt
        period >> tok KW_Class
        return $ PClassLit pos mt) <|>
    (try $ do
        pos <- getParaPosition
        n <- nameRaw tName
        period >> tok KW_This
        return $ PThisClass pos n) <|>
    try instanceCreationNPS <|>
    try (setPos PMethodInv <*> methodInvocationNPS) <|>
    try (setPos PFieldAccess <*> fieldAccessNPS) <|>
    setPos PArrayAccess <*> arrayAccessNPS <|>
    setPos PAntiQExp <*>
      javaToken (\t ->
          case t of
            AntiQExpTok s -> Just s
            _ -> Nothing)

checkClassLit :: ReturnType Pa -> P (Maybe (Type Pa))
checkClassLit (PLockType _) = fail "Lock is not a class type!"
checkClassLit (PVoidType _) = return Nothing
checkClassLit (PType _ t)    = return $ Just t


primarySuffix :: P (Exp Pa -> Exp Pa)
primarySuffix = try instanceCreationSuffix <|>
  (do pos <- getParaPosition
      try ((PArrayAccess pos .) <$> arrayAccessSuffix) <|>
        try ((PMethodInv pos .) <$> methodInvocationSuffix) <|>
        (PFieldAccess pos .) <$> fieldAccessSuffix)

instanceCreationNPS :: P (Exp Pa)
instanceCreationNPS =
    do tok KW_New
       pos <- getParaPosition
       tas <- lopt typeArgs
       ct  <- classType
       as  <- args
       mcb <- opt classBody
       return $ PInstanceCreation pos tas ct as mcb

instanceCreationSuffix :: P (Exp Pa -> Exp Pa)
instanceCreationSuffix =
     do period >> tok KW_New
        pos <- getParaPosition
        tas <- lopt typeArgs
        i   <- ident
        as  <- args
        mcb <- opt classBody
        return $ \p -> PQualInstanceCreation pos p tas i as mcb

instanceCreation :: P (Exp Pa)
instanceCreation = {- try instanceCreationNPS <|> -} do
    p <- primaryNPS
    ss <- list primarySuffix
    let icp = foldl (\a s -> s a) p ss
    case icp of
     PInstanceCreation     {} -> return icp
     PQualInstanceCreation {} -> return icp
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
        setPos PSuperFieldAccess <*> ident) <|>
    (do pos <- getParaPosition
        n <- nameRaw tName
        period >> tok KW_Super >> period
        i <- ident
        return $ PClassFieldAccess pos n i)

fieldAccessSuffix :: P (Exp Pa -> FieldAccess Pa)
fieldAccessSuffix = do
    period
    pos <- getParaPosition
    i <- ident
    return $ \p -> PPrimaryFieldAccess pos p i

fieldAccess :: P (FieldAccess Pa)
fieldAccess = {- try fieldAccessNPS <|> -} do
    p <- primaryNPS
    ss <- list primarySuffix
    let fap = foldl (\a s -> s a) p ss
    case fap of
     PFieldAccess _ fa -> return fa
     _ -> fail ""

fieldAccessExp :: P (Exp Pa)
fieldAccessExp = setPos PFieldAccess <*> fieldAccess

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
        setPos PSuperMethodCall <*> lopt nonWildTypeArgs <*> ident <*> args) <|>
    (do n <- nameRaw ambName
        f <- (do pos <- getParaPosition
                 as <- args
                 return $ \na -> PMethodCallOrLockQuery pos (mOrLName $ flattenName na) as) <|>
             (period >> do
                pos <- getParaPosition
                msp <- opt (tok KW_Super >> period)
                rts <- lopt nonWildTypeArgs
                i   <- ident
                as  <- args
                let mc = maybe (PTypeMethodCall pos) (const (PClassMethodCall pos)) msp
                return $ \na -> mc (tName $ flattenName na) rts i as)
        return $ f n)

methodInvocationSuffix :: P (Exp Pa -> MethodInvocation Pa)
methodInvocationSuffix = do
        period
        pos <- getParaPosition
        rts <- lopt nonWildTypeArgs
        i   <- ident
        as  <- args
        return $ \p -> PPrimaryMethodCall pos p rts i as

methodInvocationExp :: P (Exp Pa)
methodInvocationExp = {- try (MethodInv () <$> methodInvocationNPS) <|> -} do
    p <- primaryNPS
    ss <- list primarySuffix
    let mip = foldl (\a s -> s a) p ss
    case mip of
     PMethodInv _ _ -> return mip
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
    setPos PArrayIndex <*> (setPos PExpName <*> pure n) <*> pure e

arrayAccessSuffix :: P (Exp Pa -> ArrayIndex Pa)
arrayAccessSuffix = do
    e <- brackets exp
    pos <- getParaPosition
    return $ \ref -> PArrayIndex pos ref e

arrayAccess :: P (ArrayIndex Pa)
arrayAccess = {- try arrayAccessNPS <|> -} do
    p <- primaryNoNewArrayNPS
    ss <- list primarySuffix
    let aap = foldl (\a s -> s a) p ss
    case aap of
     PArrayAccess _ ain -> return ain
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
             return $ \ty -> PArrayCreateInit pos ty ds ai) <|>
         (do pos <- getParaPosition
             des <- list1 $ do
                      e <- brackets exp
                      p <- opt (angles argExp)
                      return (e,p)
             ds <- list (brackets empty >> opt (angles argExp))
             return $ \ty -> PArrayCreate pos ty des ds)
    return $ f t

literal :: P (Literal Pa)
literal =
    javaTokenPos $ \t p ->
      let sp = pos2sourcePos p in
      case t of
        IntTok     i -> Just (PInt     sp i)
        LongTok    l -> Just (PWord    sp l)
        DoubleTok  d -> Just (PDouble  sp d)
        FloatTok   f -> Just (PFloat   sp f)
        CharTok    c -> Just (PChar    sp c)
        StringTok  s -> Just (PString  sp s)
        BoolTok    b -> Just (PBoolean sp b)
        NullTok      -> Just (PNull    sp)
        _ -> Nothing

-- Operators

preIncDecOp, prefixOp, postfixOp :: P (Exp Pa -> Exp Pa)
preIncDecOp =
    (tok Op_PPlus  >> setPos PPreIncrement ) <|>
    (tok Op_MMinus >> setPos PPreDecrement )
prefixOp =
    (tok Op_Bang  >> setPos PPreNot      ) <|>
    (tok Op_Tilde >> setPos PPreBitCompl ) <|>
    (tok Op_Plus  >> setPos PPrePlus     ) <|>
    (tok Op_Minus >> setPos PPreMinus    )
postfixOp =
    (tok Op_PPlus  >> setPos PPostIncrement ) <|>
    (tok Op_MMinus >> setPos PPostDecrement )

assignOp :: P (AssignOp Pa)
assignOp =
    (tok Op_Equal    >> setPos PEqualA   ) <|>
    (tok Op_StarE    >> setPos PMultA    ) <|>
    (tok Op_SlashE   >> setPos PDivA     ) <|>
    (tok Op_PercentE >> setPos PRemA     ) <|>
    (tok Op_PlusE    >> setPos PAddA     ) <|>
    (tok Op_MinusE   >> setPos PSubA     ) <|>
    (tok Op_LShiftE  >> setPos PLShiftA  ) <|>
    (tok Op_RShiftE  >> setPos PRShiftA  ) <|>
    (tok Op_RRShiftE >> setPos PRRShiftA ) <|>
    (tok Op_AndE     >> setPos PAndA     ) <|>
    (tok Op_CaretE   >> setPos PXorA     ) <|>
    (tok Op_OrE      >> setPos POrA      )

infixOp :: P (Op Pa)
infixOp =
    (tok Op_Star    >> setPos PMult      ) <|>
    (tok Op_Slash   >> setPos PDiv       ) <|>
    (tok Op_Percent >> setPos PRem       ) <|>
    (tok Op_Plus    >> setPos PAdd       ) <|>
    (tok Op_Minus   >> setPos PSub       ) <|>
    (tok Op_LShift  >> setPos PLShift    ) <|>
    (tok Op_RShift  >> setPos PRShift    ) <|>
    (tok Op_RRShift >> setPos PRRShift   ) <|>
    (tok Op_LThan   >> setPos PLThan     ) <|>
    (tok Op_GThan   >> setPos PGThan     ) <|>
    (tok Op_LThanE  >> setPos PLThanE    ) <|>
    (tok Op_GThanE  >> setPos PGThanE    ) <|>
    (tok Op_Equals  >> setPos PEqual     ) <|>
    (tok Op_BangE   >> setPos PNotEq     ) <|>
    (tok Op_And     >> setPos PAnd       ) <|>
    (tok Op_Caret   >> setPos PXor       ) <|>
    (tok Op_Or      >> setPos POr        ) <|>
    (tok Op_AAnd    >> setPos PCAnd      ) <|>
    (tok Op_OOr     >> setPos PCOr       )

typeArgInfixOp :: P (Op Pa)
typeArgInfixOp =
    (tok Op_Star >> setPos PMult ) <|>
    (tok Op_Plus >> setPos PAdd  )


----------------------------------------------------------------------------
-- Types

ttype :: P (Type Pa)
ttype = try (setPos PRefType <*> refType) <|> setPos PPrimType <*> primType
         <|> setPos PAntiQType <*>
               javaToken (\t ->
                   case t of
                     AntiQTypeTok s -> Just  s
                     _              -> Nothing)

primType :: P (PrimType Pa)
primType =
    tok KW_Boolean >> setPos PBooleanT  <|>
    tok KW_Byte    >> setPos PByteT     <|>
    tok KW_Short   >> setPos PShortT    <|>
    tok KW_Int     >> setPos PIntT      <|>
    tok KW_Long    >> setPos PLongT     <|>
    tok KW_Char    >> setPos PCharT     <|>
    tok KW_Float   >> setPos PFloatT    <|>
    tok KW_Double  >> setPos PDoubleT
    -- Paragon
--     <|> tok KW_P_Actor  >> setPos ActorT
     <|> tok KW_P_Policy >> setPos PolicyT


refType :: P (RefType Pa)
refType = checkNoExtraEnd refTypeE

refTypeE :: P (RefType Pa, Int)
refTypeE = {- trace "refTypeE" -} (
    (do typ <- setPos PArrayType <*>
               (setPos PPrimType <*> primType) <*>
               list1 arrPols
        return (typ, 0))
    <|>
    (do (ct, e) <- classTypeE
        baseType <- setPos PClassRefType <*> pure ct
        if (e == 0)
         then do
          -- TODO: Correct pos?
           mps <- list arrPols
           case mps of
             [] -> return (baseType, e)
             _  -> do typ <- setPos PArrayType <*>
                             (setPos PRefType <*> pure baseType) <*> pure mps
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
nonArrayType = setPos PPrimType <*> primType <|>
    setPos PRefType <*> (setPos PClassRefType <*> classType)


classType :: P (ClassType Pa)
classType = checkNoExtraEnd classTypeE

classTypeE :: P (ClassType Pa, Int)
classTypeE = {- trace "classTypeE" $ -} do
  n <- nameRaw tName
  mtase <- opt typeArgsE
  {- trace ("mtase: " ++ show mtase) $ -}
  clt <- setPos PClassType
  case mtase of
    Just (tas, e) -> return (clt n tas, e)
    Nothing       -> return (clt n [] , 0)

returnType :: P (ReturnType Pa)
returnType = tok KW_Void   >> setPos PVoidType <|>
             tok KW_P_Lock >> setPos PLockType <|>
             setPos Type <*> ttype <?> "returnType"

classTypeList :: P [ClassType Pa]
classTypeList = seplist1 classType comma

----------------------------------------------------------------------------
-- Type parameters and arguments

typeParams :: P [TypeParam Pa]
typeParams = angles $ seplist1 typeParam comma

typeParam :: P (TypeParam Pa)
typeParam =
  (do tok KW_P_Actor >> setPos PActorParam <*> refType <*> ident) <|>
  (do tok KW_P_Policy >> setPos PPolicyParam <*> ident) <|>
  (do tok KW_P_Lock >> arrBrackets >> setPos PLockStateParam <*> ident) <|>
  (do setPos PTypeParam <*> ident <*> lopt bounds)

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
      wcArg <- setPos PWildcard <*> opt wildcardBound
      (rest, e) <- typeArgsEnd 0
      return (wcArg:rest, e)) <|>

  (do lArg <- setPos PActualArg <*> parens (setPos PActualLockState <*> seplist1 lock comma)
      (rest, e) <- typeArgsEnd 0
      return (lArg:rest, e)) <|>

  (try $ do (rt, er)  <- refTypeE
            (rest, e) <- typeArgsEnd er
            tArg <- case nameOfRefType rt of
                      Just n -> setPos PActualName <*> pure (ambName (flattenName n)) -- keep as ambiguous
                      _ -> setPos PActualType <*> pure rt
            actArg <- setPos PActualArg <*> pure tArg
            return (actArg:rest, e)) <|>

  (do eArg <- setPos PActualArg <*> (setPos PActualExp <*> argExp)
      (rest, e) <- typeArgsEnd 0
      return (eArg:rest, e))

      where nameOfRefType :: RefType Pa -> Maybe (Name Pa)
            nameOfRefType (PClassRefType _ (PClassType _ n tas)) =
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
argExp1 = setPos PPolicyExp <*> policyExp
          <|> try methodInvocationExp
          <|> try fieldAccessExp
          <|> setPos PExpName <*> nameRaw eName

-- ****************

argExpSuffix :: P (Exp Pa -> Exp Pa)
argExpSuffix =
    (do op <- typeArgInfixOp
        e2 <- argExp
        binop <- setPos PBinOp -- TODO: Does this make sense?
        return $ \e1 -> binop e1 op e2) <|> return id

wildcardBound :: P (WildcardBound Pa)
wildcardBound =
      tok KW_Extends >> setPos PExtendsBound <*> refType
  <|> tok KW_Super >> setPos PSuperBound <*> refType

nonWildTypeArgs :: P [NonWildTypeArgument Pa]
nonWildTypeArgs = typeArgs >>= mapM checkNonWild
  where checkNonWild (PActualArg _ arg) = return arg
        checkNonWild _ = fail "Use of wildcard in non-wild context"


--nonWildTypeArgs :: P [NonWildTypeArgument ()]
--nonWildTypeArgs = angles $ seplist nonWildTypeArg (tok Comma)

----------------------------------------------------------------------------
-- Names

nameRaw :: ([Ident Pa] -> Name Pa) -> P (Name Pa)
nameRaw nf =
    nf <$> seplist1 ident period <|>
        javaTokenPos (\t p -> case t of
          AntiQNameTok s -> Just $ PAntiQName (pos2sourcePos p) s
          _ -> Nothing)

name :: P (Name Pa)
name = nameRaw ambName

ident :: P (Ident Pa)
ident = javaTokenPos $ \t p -> case t of
    IdentTok s -> Just $ PIdent (pos2sourcePos p) (B.pack s)
    AntiQIdentTok s -> Just $ PAntiQIdent (pos2sourcePos p) s
    _ -> Nothing

----------------------------------------------------------------------------
-- Policies

policy :: P (Policy Pa)
policy = postfixExpNES -- Policy <$> policyLit <|> PolicyRef <$> (tok Op_Tilde >> name)

policyExp :: P (PolicyExp Pa)
policyExp =
  try (setPos PPolicyLit <*> (braces $ seplist clause semiColon)) <|>
  setPos PPolicyLit <*> (braces colon >> return []) <|>
  tok KW_P_Policyof >> parens (setPos PPolicyOf <*> ident <|>
                               (do pol <- setPos PPolicyThis
                                   const pol <$> tok KW_This))

clause :: P (Clause Pa)
clause = do
    pos <- getParaPosition
    vs <- lopt $ parens $ seplist cvardecl comma
    ch <- chead
    ats <- lopt $ colon >> seplist atom comma
    let avdis = map (\(PClauseVarDecl _ _ i) -> i) $
                 case ch of
                   PClauseDeclHead _ cvd -> cvd:vs
                   _ -> vs
        ch' = genActorVars avdis ch
        ats' = genActorVars avdis ats
    return $ PClause pos vs ch' ats'


cvardecl :: P (ClauseVarDecl Pa)
cvardecl = setPos PClauseVarDecl <*> refType <*> ident

chead :: P (ClauseHead Pa)
chead = try (setPos PClauseDeclHead <*> cvardecl) <|>
        setPos PClauseVarHead <*> actor

lclause :: P (LClause Pa)
lclause = do
    pos <- getParaPosition
    qs <- lopt $ parens $ seplist cvardecl comma
    mh <- lclauseHead
    as <- lopt $ colon >> seplist atom comma
    let avdis = map (\(PClauseVarDecl _ _ i) -> i) qs
        as'   = genActorVars avdis as

    case mh of
      Just h -> do
              let [h'] = genActorVars avdis [h]
              return $ PLClause pos qs h' as'
      Nothing -> return $ PConstraintClause pos qs as'

lclauseHead :: P (Maybe (Atom Pa))
lclauseHead =
  (tok Op_Minus >> return Nothing) <|>
   (Just <$> atom)

atom :: P (Atom Pa)
atom = do
    setPos PAtom <*> nameRaw lName
                <*> (lopt $ parens $ seplist actor comma)

-- We parse everything as actorNames, and post-process
-- them into Vars
actor :: P (Actor Pa)
actor = setPos PActor <*> actorName -- <|>
--        setPos Var <*> actorVar -- (tok Op_Query >> ident)

actorName :: P (ActorName Pa)
actorName = setPos PActorName <*> nameRaw eName

--actorVar :: P (Ident Pa)
--actorVar = javaTokenPos $ \t p -> case t of
--    VarActorTok s -> Just $ Ident (pos2sourcePos p) (B.pack s)
--    _ -> Nothing

lock :: P (Lock Pa)
lock = do
    setPos PLock <*> nameRaw lName
                <*> (lopt $ parens $ seplist actorName comma)

lockProperties :: P (LockProperties Pa)
lockProperties = do braces $ setPos PLockProperties
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
pos2sourcePos :: (Int, Int) -> Pa
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
checkConstrs (PClassDecl _ _ i _ _ _ cb) = do
    let errs = [ ci | PConstructorDecl _ _ _ ci _ _ _ <- universeBi cb, ci /= i ]
    if null errs then return ()
     else fail $ "Declaration of class " ++ prettyPrint i
                  ++ " cannot contain constructor with name "
                  ++ prettyPrint (head errs)
checkConstrs _ = panic (parserModule ++ ".checkConstrs")
                 "Checking constructors of Enum decl"

-----------------------------------------------------
-- Making the meaning of a name explicit

ambName :: [Ident a] -> Name a
ambName = mkUniformName_ PAmbName

-- A package name can only have a package name prefix
pName :: [Ident a] -> Name a
pName = mkUniformName_ AST.PName

-- A package-or-type name has a package-or-type prefix
pOrTName :: [Ident a] -> Name a
pOrTName = mkUniformName_ POrTName

-- A type name has a package-or-type prefix
tName :: [Ident a] -> Name a
tName = mkName_ TName POrTName

-- Names with ambiguous prefixes
eName, lName, eOrLName, mOrLName :: [Ident a] -> Name a
eName = mkName_ EName AmbName
lName = mkName_ LName AmbName
eOrLName = mkName_ EOrLName AmbName
mOrLName = mkName_ MOrLName AmbName


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
          gen (PClassRefType pos (PClassType _ (Name _ TName Nothing i) []))
              | i `elem` parIs = PTypeVariable pos i
          gen rt = rt

          genA :: ActorName Pa -> ActorName Pa
          genA (PActorName pos (Name _ EName Nothing i))
               | Just rt <- lookup i actIs = PActorTypeVar pos rt i
          genA a = a

          genP :: Exp Pa -> Exp Pa
          genP (PExpName pos (Name _ EName Nothing i))
              | i `elem` polIs = PPolicyExp pos (PPolicyTypeVar pos i)
          genP e = e

          genL :: Lock Pa -> Lock Pa
          genL (PLock pos (Name _ LName Nothing i) [])
              | i `elem` locIs = PLockVar pos i
          genL l = l

          parIs = [ i | PTypeParam _ i _ <- pars ]
          locIs = [ i | PLockStateParam _ i <- pars ]
          polIs = [ i | PPolicyParam _ i <- pars ]
          actIs = [ (i, rt) | PActorParam _ rt i <- pars ]


-- Generalization of variables in a policy literal
genActorVars :: Data x => [Ident Pa] -> x -> x
genActorVars is = transformBi gen
  where --gen :: Actor a -> Actor a
        gen (PActor _ (PActorName _ (Name _ _ Nothing i)))
            | i `elem` is = PVar (ann i) i
        gen ac = ac

-- TODO: Temporary ann definiton. The real one is probably form Annotated.hs
ann = undefined

--------------------------------------------------------------
-- Resolving precedences

builtInPrecs :: [(Op (), Int)]
builtInPrecs =
    map (,9) [PMult   (), PDiv    (), PRem     ()            ] ++
    map (,8) [PAdd    (), PSub    ()                         ] ++
    map (,7) [PLShift (), PRShift (), PRRShift ()            ] ++
    map (,6) [PLThan  (), PGThan  (), PLThanE  (), PGThanE ()] ++
    map (,5) [PEqual  (), PNotEq  ()                         ] ++
    [(PAnd  (), 4) ,
     (POr   (), 3),
     (PXor  (), 2),
     (PCAnd (), 1),
     (PCOr  (), 0)]

instanceOfPrec :: Int
instanceOfPrec = 6 -- same as comparison ops

-- ### This section has been removed because the new AST deals with SourcePos
-- ### differently.

-- dropData :: Op a -> Op NoFieldExt
-- dropData (Mult _ _)    = Mult    () ()
-- dropData (Div _ _)     = Div     () ()
-- dropData (Rem _ _)     = Rem     () ()
-- dropData (Add _ _)     = Add     () ()
-- dropData (Sub _ _)     = Sub     () ()
-- dropData (LShift _ _)  = LShift  () ()
-- dropData (RShift _ _)  = RShift  () ()
-- dropData (RRShift _ _) = RRShift () ()
-- dropData (LThan _ _)   = LThan   () ()
-- dropData (GThan _ _)   = GThan   () ()
-- dropData (LThanE _ _)  = LThanE  () ()
-- dropData (GThanE _ _)  = GThanE  () ()
-- dropData (Equal _ _)   = Equal   () ()
-- dropData (NotEq _ _)   = NotEq   () ()
-- dropData (And _ _)     = And     () ()
-- dropData (Or _ _)      = Or      () ()
-- dropData (Xor _ _)     = Xor     () ()
-- dropData (CAnd _ _)    = CAnd    () ()
-- dropData (COr _ _)     = COr     () ()

-- -- TODO: Fix positions?
-- fixPrecs :: Exp Pa -> Exp Pa
-- fixPrecs (BinOp pos a op2 z) =
--     let e = fixPrecs a -- recursively fix left subtree
--         getPrec op = fromJust $ lookup op builtInPrecs
--         fixup p1 p2 y pre =
--             if p1 >= p2
--              then BinOp pos e op2 z -- already right order
--              else pre (fixPrecs $ BinOp pos y op2 z)
--     in case e of
--          BinOp pos' x op1 y   -> fixup (getPrec . dropData $ op1)  (getPrec . dropData $ op2) y (BinOp pos' x op1)
--          InstanceOf pos' y rt -> fixup instanceOfPrec (getPrec . dropData $ op2) y (flip (InstanceOf pos') rt)
--          _ -> BinOp pos e op2 z
-- fixPrecs e = e
