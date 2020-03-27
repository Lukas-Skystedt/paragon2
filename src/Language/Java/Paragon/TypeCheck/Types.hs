{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Language.Java.Paragon.TypeCheck.Types where

import Security.InfoFlow.Policy.FlowLocks.Policy (MetaPolicy(..),VarPolicy(..),Clause(..),Atom(..),Policy(..))

import Language.Java.Paragon.SyntaxTTG hiding (Clause(..))
import Language.Java.Paragon.Decorations.DecorationTypes
import Language.Java.Paragon.Decorations.NoDecoration
import Language.Java.Paragon.Decorations.PaDecoration
import Language.Java.Paragon.Pretty
import Language.Java.Paragon.Interaction
import Language.Java.Paragon.Error()
import Language.Java.Paragon.SourcePos

import {-# SOURCE #-}Language.Java.Paragon.PolicyLang
import Language.Java.Paragon.TypeCheck.NullAnalysis

import qualified Data.ByteString.Char8 as B
import Data.Maybe (isJust, fromJust)
import Control.Applicative (Applicative, (<$>), (<*>))
import Data.List ((\\))

import Data.Data (Data, Typeable)

import Prelude hiding ((<>))
import qualified Control.Monad.Fail as Fail

-- Make pattern synonyms on the form
-- > pattern TcCompilationUnit typ mpd id td = CompilationUnit typ mpd id td
$(makePatternSyns "Tc"
  (allDataConstructors \\ ['ActualType, 'ActualLockState, 'ArrayType, 'VoidType])
  [p| () |])


typesModule :: String
typesModule = typeCheckerBase ++ ".Types"

-- | TODO: Better name?
-- | Wrapper type to distinguish Paragon-specific types from Java-compatible
-- types.
data T = ParagonType (Type TC)
       | JavaType (Type TC)

deriving instance Eq T
deriving instance Ord T
deriving instance Show T
deriving instance Data T
deriving instance Typeable T

-- | Where annotation is not applicable, replace with Nothing.
notAppl :: Functor f => f a -> f T
notAppl = error "notAppl: not implemented. TOOD: figure out Maybe T vs T"--fmap (const Nothing)

-- | Converts a statetype to an annotated AST type as being a non-native type,
-- having removed the state information.
toT :: TcStateType -> T
toT sty = JavaType (unStateType sty)

setNative :: Bool -> T -> T
-- setNative b = fmap $ \(t,_) -> (t,b)

setNative b t = case b of
                  True  -> ParagonType (getType t)
                  False -> JavaType    (getType t)
  where getType (JavaType t)    = t
        getType (ParagonType t) = t


data TcStateType
    = TcInstance (RefType TC) TypedActorIdSpec [TypedActorIdSpec] NullType -- ^ [ActorIdentity] Instance fresh final actor fields
--    | TcActorIdT TypedActorIdSpec
    | TcPolicyPolT ActorPolicyBounds
    | TcLockT [TcLock]             -- ^ List of looks checked in the expression
    | TcTypeNT (Type TC) NullType       -- ^ Simply the type with nullpointer information. NT - NullType
  -- deriving (Eq, Show, Data, Typeable)

deriving instance Eq TcStateType
deriving instance Ord TcStateType
deriving instance Show TcStateType
deriving instance Data TcStateType
deriving instance Typeable TcStateType

instance Pretty TcStateType where
  pretty tcst =
      case tcst of
        --TcActorIdT aid -> text "actor[" <> pretty aid <> text "]"
        TcPolicyPolT p -> text "policy[" <> pretty p <> text "]"
        TcLockT ls -> (hsep $ text "lock[" : punctuate (text ",") (map pretty ls)) <> text "]"
        TcInstance ct aid aids _ -> pretty ct <> char '@' <> pretty aid <>
                                 (hsep $ char '{' : punctuate (char ',') (map pretty aids)) <> char '}' --TODOY prettyprint nulltype
        TcTypeNT ty nt -> pretty ty <> pretty nt

------------------------------------
-- Constructors

booleanT, byteT, shortT, intT, longT,
 charT, floatT, doubleT, {-actorT,-} policyT :: (Type TC)
booleanT = TcPrimType (BooleanT defaultPos)
byteT    = TcPrimType (ByteT defaultPos)
shortT   = TcPrimType (ShortT defaultPos)
intT     = TcPrimType (IntT defaultPos)
longT    = TcPrimType (LongT defaultPos)
charT    = TcPrimType (CharT defaultPos)
floatT   = TcPrimType (FloatT defaultPos)
doubleT  = TcPrimType (DoubleT defaultPos)
--actorT   = TcPrimType (ActorT   defaultPos)
policyT  = TcPrimType (PolicyT defaultPos)

-- | Takes a regular type and converts it to a type with state.
-- If the type is a null reference it's result will have state
-- @(MaybeNull, Committed)@, otherwise it is @(NotNull, Committed)@.
stateType :: Type TC -> TcStateType
stateType t@(TcRefType TcNullT) = TcTypeNT t (MaybeNull, Committed)
stateType t = TcTypeNT t (NotNull, Committed)

unStateType :: TcStateType -> (Type TC)
unStateType tcst = case tcst of
                     TcInstance rt _ _ _ -> TcRefType rt
--                     TcActorIdT    _   -> actorT
                     TcPolicyPolT  _   -> policyT
                     TcLockT       _   -> booleanT
                     TcTypeNT      t _  -> t

nullFromStateType :: TcStateType -> Maybe NullType
nullFromStateType tcst = case tcst of
                     TcInstance _ _ _ nt -> Just nt
--                     TcActorIdT    _   -> Nothing
                     TcPolicyPolT  _   -> Nothing
                     TcLockT       _   -> Nothing
                     TcTypeNT      _ nt  -> Just nt

nullableFromStateType :: TcStateType -> Bool
nullableFromStateType tcst = case nullFromStateType tcst of
                               (Just (MaybeNull, _)) -> True
                               _ -> False

setNullInStateType :: TcStateType -> NullType -> TcStateType
setNullInStateType tcst nt = case tcst of
                               TcInstance rt aid as _ -> TcInstance rt aid as nt
                               TcTypeNT           t _ -> TcTypeNT t nt
                               st                     -> st

nullT, voidT :: Type TC
nullT    = TcRefType TcNullT
voidT    = TcVoidType

--actorIdT :: TypedActorIdSpec -> TcStateType
--actorIdT = TcActorIdT

policyPolT :: ActorPolicyBounds -> TcStateType
policyPolT = TcPolicyPolT

lockT :: [TcLock] -> TcStateType
lockT = TcLockT

instanceT :: RefType TC -> TypedActorIdSpec -> [TypedActorIdSpec] -> NullType -> TcStateType
instanceT = TcInstance

{-
clsTypeWArg :: Name () -> [TcTypeArg] -> (Type TC)
clsTypeWArg n = TcClassT n
-}

clsType :: Ident TC -> ClassType TC
clsType = qualClsType . return

qualClsType :: [Ident TC] -> ClassType TC
qualClsType is = TcClassType (mkName_ TName PName is) []

--nameToClsType :: Name () -> TcClassType
--nameToClsType (Name _ is) = TcClassT $ map (\i -> (i,[])) is
--nameToClsType _ = panic (typesModule ++ ".nameToClsType")
--                  "AntiQName should never appear in an AST being type-checked"

stringT, objectT :: ClassType TC
stringT = qualClsType $ map (Ident defaultPos . B.pack)
  ["java","lang","String"]
objectT = qualClsType $ map (Ident defaultPos . B.pack)
  ["java","lang","Object"]

nullExnT :: (Type TC)
nullExnT = TcRefType $ TcClassRefType (qualClsType $
  map (Ident defaultPos . B.pack)
  ["java","lang","NullPointerException"])

-- promoting

clsTypeToType :: ClassType TC -> (Type TC)
clsTypeToType = TcRefType . TcClassRefType

arrayType :: (Type TC) -> ActorPolicy -> (Type TC)
arrayType = (TcRefType .) . TcArrayType

mkArrayType :: (Type TC) -> [ActorPolicy] -> (Type TC)
mkArrayType = foldr (flip arrayType)


-----------------------------------
-- Destructors

-- Invariant: First argument is a class type
typeName :: (Type TC) -> Maybe (Name TC)
typeName (TcRefType (TcClassRefType (TcClassType n tas))) =
--         let (is, args) = unzip pn in
         if null tas then Just (mkUniformName_ AmbName $ flattenName n) else Nothing
typeName _ = Nothing

typeName_ :: (Type TC) -> Name TC
typeName_ (TcRefType (TcClassRefType (TcClassType n _tas))) = mkUniformName_ AmbName $ flattenName n
--    let (is, _) = unzip pn in mkUniformName_ AmbName is
typeName_ t = error $ "typeName_: Not a class type: " ++ show t

--typeName_ typ = case typeName typ of
--                  Just n -> n
--                  Nothing -> error $ "typeName_: " ++ show typ

isClassType, isRefType, isPrimType, isNullType, maybeNull :: TcStateType -> Bool
isClassType (TcTypeNT (TcRefType (TcClassRefType TcClassType{})) _) = True
isClassType TcInstance{} = True
isClassType _ = False

isRefType (TcTypeNT (TcRefType _) _) = True
isRefType TcInstance{} = True
isRefType _ = False

mRefType :: TcStateType -> Maybe (RefType TC)
mRefType (TcTypeNT (TcRefType rTy) _) = Just rTy
mRefType (TcInstance rTy _ _ _) = Just rTy
mRefType _ = Nothing

isPrimType (TcTypeNT (TcPrimType _) _) = True
isPrimType _ = False

mNameRefType :: (RefType TC) -> Maybe (Name TC)
mNameRefType (TcClassRefType (TcClassType n as)) =
    if null as then Just (mkUniformName_ AmbName $ flattenName n) else Nothing
mNameRefType _ = Nothing

isNullType (TcTypeNT (TcRefType TcNullT) _) = True
isNullType _ = False

maybeNull (TcTypeNT _ nt) = nullable nt
maybeNull (TcInstance _ _ _ nt) = nullable nt
maybeNull _ = False

mActorId :: TcStateType -> Maybe TypedActorIdSpec
mActorId (TcInstance _ aid _ _) = Just aid
--mActorId (TcActorIdT aid) = Just aid
mActorId _ = Nothing

mLocks :: TcStateType -> Maybe [TcLock]
mLocks (TcLockT ls) = Just ls
mLocks _ = Nothing

mPolicyPol :: TcStateType -> Maybe ActorPolicyBounds
mPolicyPol (TcPolicyPolT p) = Just p
mPolicyPol _ = Nothing

isLockType :: TcStateType -> Bool
isLockType = isJust . mLocks

isActorType :: TcStateType -> Bool
isActorType = isJust . mActorId

isPolicyType :: TcStateType -> Bool
isPolicyType = isJust . mPolicyPol

isArrayType :: TcStateType -> Bool
isArrayType = isJust . mArrayType . unStateType

mArrayType :: (Type TC) -> Maybe ((Type TC), [ActorPolicy])
mArrayType (TcRefType (TcArrayType ty p)) = Just $
    case mArrayType ty of
      Nothing -> (ty, [p])
      Just (t, ps) -> (t, p:ps)
mArrayType _ = Nothing

mClassType :: (Type TC) -> Maybe (ClassType TC)
mClassType (TcRefType (TcClassRefType ct@TcClassType{})) = Just ct
mClassType _ = Nothing

mInstanceType :: TcStateType -> Maybe ((RefType TC), TypedActorIdSpec, [TypedActorIdSpec], NullType)
mInstanceType (TcInstance rt aid aids nt) = Just (rt, aid, aids, nt)
mInstanceType _ = Nothing

-------------------------------------------
-- Type operations

widenConvert :: PrimType PA -> [PrimType PA]
widenConvert pt = case pt of
   FloatT  pos -> map ($ pos) [DoubleT]
   LongT   pos -> map ($ pos) [DoubleT, FloatT]
   IntT    pos -> map ($ pos) [DoubleT, FloatT, LongT]
   ShortT  pos -> map ($ pos) [DoubleT, FloatT, LongT, IntT]
   CharT   pos -> map ($ pos) [DoubleT, FloatT, LongT, IntT]
   ByteT   pos -> map ($ pos) [DoubleT, FloatT, LongT, IntT, ShortT]
   _           -> []

narrowConvert :: PrimType PA -> [PrimType PA]
narrowConvert pt = case pt of
   DoubleT pos -> map ($ pos) [ByteT, ShortT, CharT, IntT, LongT, FloatT]
   FloatT  pos -> map ($ pos) [ByteT, ShortT, CharT, IntT, LongT]
   LongT   pos -> map ($ pos) [ByteT, ShortT, CharT, IntT]
   IntT    pos -> map ($ pos) [ByteT, ShortT, CharT]
   CharT   pos -> map ($ pos) [ByteT, ShortT]
   ShortT  pos -> map ($ pos) [ByteT, CharT]
   _           -> []

widenNarrowConvert :: PrimType PA -> [PrimType PA]
widenNarrowConvert (ByteT pos) = [CharT pos]
widenNarrowConvert _           = []

box :: --XName TC ~ XIdent PA =>
  PrimType PA -> Maybe (ClassType TC)
box pt = let mkClassType str spos =
                 Just $ TcClassType
                          (mkName_ TName PName $
                            map (Ident spos . B.pack)
                              ["java", "lang", str]) []
         in case pt of
              BooleanT spos -> mkClassType "Boolean" spos
              ByteT    spos -> mkClassType "Byte" spos
              ShortT   spos -> mkClassType "Short" spos
              CharT    spos -> mkClassType "Character" spos
              IntT     spos -> mkClassType "Integer" spos
              LongT    spos -> mkClassType "Long" spos
              FloatT   spos -> mkClassType "Float" spos
              DoubleT  spos -> mkClassType "Double" spos
              _ -> Nothing

unbox :: ClassType TC -> Maybe (PrimType TC)
unbox (TcClassType n@(Name spos _ _ _) _) =
    case map (B.unpack . unIdent) $ flattenName n of
      ["java", "lang", "Boolean"  ] -> Just $ BooleanT spos
      ["java", "lang", "Byte"     ] -> Just $ ByteT    spos
      ["java", "lang", "Character"] -> Just $ CharT    spos
      ["java", "lang", "Short"    ] -> Just $ ShortT   spos
      ["java", "lang", "Integer"  ] -> Just $ IntT     spos
      ["java", "lang", "Long"     ] -> Just $ LongT    spos
      ["java", "lang", "Float"    ] -> Just $ FloatT   spos
      ["java", "lang", "Double"   ] -> Just $ DoubleT  spos
      _ -> Nothing
--unbox TcNullT = Nothing


unboxType :: TcStateType -> Maybe (PrimType TC)
unboxType sty | TcRefType (TcClassRefType ct) <- unStateType sty = unbox ct
unboxType _ = Nothing

isNumConvertible :: TcStateType -> Bool
isNumConvertible sty =
    unStateType sty `elem` [byteT, shortT, intT, longT, charT, floatT, doubleT] ||
    case unboxType sty of
      Just t | t `elem` map ($ aOfPrimType t) [ByteT, ShortT, IntT, LongT, CharT, FloatT, DoubleT] -> True
      _ -> False

isIntConvertible :: TcStateType -> Bool
isIntConvertible sty =
    unStateType sty `elem` [byteT, shortT, intT, longT, charT] ||
    case unboxType sty of
      Just t | t `elem` map ($ aOfPrimType t) [ByteT, ShortT, IntT, LongT, CharT] -> True
      _ -> False

isBoolConvertible :: TcStateType -> Bool
isBoolConvertible t = unStateType t == booleanT -- includes lock types
                      || unboxType t == Just (BooleanT defaultPos)


unaryNumPromote :: TcStateType -> Maybe (PrimType TC)
unaryNumPromote sty
    | TcPrimType pt <- unStateType sty  = numPromote pt
    | Just    pt <- unboxType   sty  = numPromote pt
    | otherwise = Nothing

    where numPromote :: PrimType TC -> Maybe (PrimType TC)
          numPromote pt
              | pt `elem` map ($ aOfPrimType pt) [LongT, FloatT, DoubleT] = Just pt
              | pt `elem` map ($ aOfPrimType pt) [ByteT, ShortT, IntT, CharT] = Just $ IntT (aOfPrimType pt)
              | otherwise = Nothing

unaryNumPromote_ :: TcStateType -> TcStateType
unaryNumPromote_ = stateType . TcPrimType . fromJust . unaryNumPromote

binaryNumPromote :: TcStateType -> TcStateType -> Maybe (PrimType TC)
binaryNumPromote t1 t2 = do
    pt1 <- unaryNumPromote t1
    pt2 <- unaryNumPromote t2
    return $ max pt1 pt2

binaryNumPromote_ :: TcStateType -> TcStateType -> TcStateType
binaryNumPromote_ t1 t2 = stateType . TcPrimType . fromJust $ binaryNumPromote t1 t2

class (Functor m, Applicative m, Monad m, Fail.MonadFail m) => HasSubTyping m where
  subTypeOf :: (RefType TC) -> (RefType TC) -> m Bool


---------------------------------------------
-- Pretty printing

-- TODO: Check that the implementations in 'Pretty.hs' is sufficient to replace
-- the below definitions.

-- instance Pretty TcStateType where
--   pretty tcst =
--       case tcst of
--         --TcActorIdT aid -> text "actor[" <> pretty aid <> text "]"
--         TcPolicyPolT p -> text "policy[" <> pretty p <> text "]"
--         TcLockT ls -> hsep (text "lock[" : punctuate (text ",") (map pretty ls)) <> text "]"
--         TcInstance ct aid aids _ -> pretty ct <> char '@' <> pretty aid <>
--                                  hsep (char '{' : punctuate (char ',') (map pretty aids)) <> char '}' --TODOY prettyprint nulltype
--         TcType ty nt -> pretty ty <> pretty nt

-- instance Pretty (Type TC) where
--   pretty tct =
--       case tct of
--         TcPrimType pt -> pretty pt
--         TcRefType rt -> pretty rt
--         TcVoidT -> text "void"

-- instance Pretty (RefType TC) where
--   pretty tcrt =
--       case tcrt of
--         TcClsRefT ct -> pretty ct
--         TcArrayType {} -> let (bt, suff) = ppArrayType (TcRefType tcrt) in bt <> suff
--         TcTypeVariable i -> pretty i
--         TcNullT -> text "<null>"

-- ppArrayType :: (Type TC) -> (Doc, Doc)
-- ppArrayType (TcRefType (TcArrayType ty pol)) =
--     let (bt, suff) = ppArrayType ty
--     in (bt, text "[]" <> char '<' <> pretty pol <> char '>' <> suff)
-- ppArrayType ty = (pretty ty, empty)


-- instance Pretty TcClassType where
--   pretty (TcClassT n tas) =
--       pretty n <> ppTypeParams tas
-- --      hcat . punctuate (char '.') $ map (\(i,tas) -> pretty i <> ppTypeParams tas) iargs

-- instance Pretty TcTypeArg where
--   pretty (TcActualType t) = pretty t
--   pretty (TcActualPolicy p) = pretty p
--   pretty (TcActualActor aid) = pretty aid
--   pretty (TcActualLockState ls) = ppArgs ls


-- ppTypeParams :: Pretty a => [a] -> Doc
-- ppTypeParams [] = empty
-- ppTypeParams tps = char '<'
--     <> hsep (punctuate comma (map pretty tps))
--     <> char '>'

-- ppArgs :: Pretty a => [a] -> Doc
-- ppArgs = parens . hsep . punctuate comma . map pretty

-- | Convert a 'Name PA' to 'Name TC'. TODO: These *should* be the same type; we
-- should need to convert.
namePaToTc :: Name PA -> Name TC
namePaToTc (Name sp nt mn i) = Name sp nt (namePaToTc <$> mn) (identPaToTc i)
namePaToTc (AntiQName sp s) = AntiQName sp s

-- | Convert an 'Ident PA' to 'Ident TC'.
identPaToTc :: Ident PA -> Ident TC
identPaToTc (Ident sp bs) = Ident sp bs
identPaToTc (AntiQIdent sp bs) = AntiQIdent sp bs

-- | Convert a 'Name PA' to 'Name TC'. TODO: These *should* be the same type; we
-- should need to convert.
nameTcToPa :: Name TC -> Name PA
nameTcToPa (Name sp nt mn i) = Name sp nt (nameTcToPa <$> mn) (identTcToPa i)
nameTcToPa (AntiQName sp s) = AntiQName sp s

-- | Convert an 'Ident PA' to 'Ident TC'.
identTcToPa :: Ident TC -> Ident PA
identTcToPa (Ident sp bs) = Ident sp bs
identTcToPa (AntiQIdent sp bs) = AntiQIdent sp bs

primTypePaToTc :: PrimType PA -> PrimType TC
primTypePaToTc (BooleanT sp) = BooleanT sp
primTypePaToTc (ByteT    sp) = ByteT    sp
primTypePaToTc (ShortT   sp) = ShortT   sp
primTypePaToTc (IntT     sp) = IntT     sp
primTypePaToTc (LongT    sp) = LongT    sp
primTypePaToTc (CharT    sp) = CharT    sp
primTypePaToTc (FloatT   sp) = FloatT   sp
primTypePaToTc (DoubleT  sp) = DoubleT  sp
primTypePaToTc (ActorT   sp) = ActorT   sp
primTypePaToTc (PolicyT  sp) = PolicyT  sp

--------------------------------------------------------------------------------
-- Old Decoration file below
--------------------------------------------------------------------------------

-- | Type checking. AST type index for the result of the type checking phase.
data TC deriving Data

-- | Extension constructors for 'TypeArg TC'.
--
-- This type should not be used outside of this module. Instead, use the
-- following pattern synonyms:
-- * 'TcActualType',
-- * 'TcActualPolicy',
-- * 'TcActualActor', and
-- * 'TcActualLockState'.
data TcDecTypeArg
  = TcDecActualType (RefType TC)
  | TcDecActualPolicy ActorPolicy
  | TcDecActualActor TypedActorIdSpec
  | TcDecActualLockState [TcLock]

deriving instance Eq TcDecTypeArg
deriving instance Ord TcDecTypeArg
deriving instance Show TcDecTypeArg
deriving instance Data TcDecTypeArg
deriving instance Typeable TcDecTypeArg

type instance XType            TC = NoFieldExt
type instance XPrimType        TC = SourcePos
type instance XRefType         TC = NoFieldExt
type instance XClassType       TC = NoFieldExt

type instance XIdent           TC = SourcePos
type instance XName            TC = SourcePos
type instance XTypeArgumentExp TC = TcDecTypeArg

pattern TcActualType rt      = TypeArgumentExp (TcDecActualType rt)
pattern TcActualPolicy ap    = TypeArgumentExp (TcDecActualPolicy ap)
pattern TcActualActor taid   = TypeArgumentExp (TcDecActualActor taid)
pattern TcActualLockState ls = TypeArgumentExp (TcDecActualLockState ls)

type instance XRefTypeExp TC = NoFieldExt

-- Note: Not 'Void', it is used for representing the Paragon type void
type instance XTypeExt TC = NoFieldExt
pattern TcVoidType = TypeExt ()

pattern TcNullT = RefTypeExp ()

type instance XRefTypeArrayType TC = ActorPolicy
pattern TcArrayType typ pol = ArrayType pol typ

-- Derive type instances on the form
-- > type instance XCompilationUnit PTE = NoFieldExt
-- or
-- > type instance XExp PTE = TcPlaceHolder
-- for all extension fields.
--
-- The former is for types that don't have Paragon types, the latter for types
-- that do.

$(makeTypeInsts ''TC ''NoFieldExt
  [ ''XCompilationUnit, ''XPackageDecl, ''XImportDecl, ''XTypeDecl, ''XClassDecl
  , ''XClassBody, ''XEnumBody, ''XEnumConstant, ''XInterfaceDecl, ''XInterfaceBody
  , ''XDecl, ''XType, {-''XClassType,--} ''XRefType, ''XReturnType, ''XVarInit
  ])


$(makeTypeInsts ''TC ''T
  [ ''XMemberDecl, ''XVarDecl, ''XVarDeclId, ''XFormalParam, ''XMethodBody
  , ''XConstructorBody, ''XExplConstrInv, ''XMod, ''XBlock, ''XBlockStm
  , ''XStm, ''XCatch, ''XSwitchBlock, ''XSwitchLabel, ''XForInit
  , ''XExceptionSpec, ''XExp, ''XLiteral, ''XOp, ''XAssignOp, ''XLhs
  , ''XArrayIndex, ''XFieldAccess, ''XMethodInvocation, ''XArrayInit
  , ''XNonWildTypeArgument, ''XWildcardBound, ''XTypeParam
  , ''XPolicyExp, ''XLockProperties, ''XClause, ''XClauseVarDecl, ''XClauseHead
  , ''XLClause, ''XActor, ''XActorName, ''XAtom, ''XLock
  ]
  )
