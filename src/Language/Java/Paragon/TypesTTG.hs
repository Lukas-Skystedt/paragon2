module Language.Java.Paragon.TypesTTG where
import Language.Java.Paragon.SyntaxTTG
import Language.Java.Paragon.PolicyLang
import Language.Java.Paragon.Pretty
import Language.Java.Paragon.SourcePos
-- TODO: probably should not use Pa here, find a way of doing it nicer.
import Language.Java.Paragon.Decorations.PaDecoration (Pa)

import qualified Data.ByteString.Char8 as B
import Language.Java.Paragon.TypeCheck.NullAnalysis
import Data.Maybe (fromJust, isJust)
import Prelude hiding ((<>))

-- | Used for annotated AST; a maybe-pair consisting of the actual type and
-- a boolean indicating if the type is native (to Paragon) or not.
type T = Maybe (TcType, Bool) -- Used for annotated AST

data TcStateType
    = TcInstance TcRefType (TypedActorIdSpec TcRefType) [TypedActorIdSpec TcRefType] NullType -- ^ [ActorIdentity] Instance fresh final actor fields
--    | TcActorIdT TypedActorIdSpec
    | TcPolicyPolT (ActorPolicyBounds TcRefType)
    | TcLockT [TcLock TcRefType]             -- ^ List of looks checked in the expression
    | TcType TcType NullType       -- ^ Simply the type with nullpointer information
  deriving (Eq, Show)


data TcType
    = TcPrimT (PrimType Pa) | TcRefT TcRefType | TcVoidT --  TcLockRetT
     -- --  TcActorIdT ActorId | TcPolicyPolT ActorPolicy | TcLockT [TcLock]
  deriving (Eq, Ord, Show)

data TcRefType
    = TcClsRefT TcClassType
    | TcArrayT TcType (ActorPolicy TcRefType)
    | TcTypeVar B.ByteString
    | TcNullT
  deriving (Eq, Ord, Show)

data TcClassType
    = TcClassT (Name Pa) [TcTypeArg] -- [ActorId] -- Ignore wildcards for now
  deriving (Eq, Ord, Show)

data TcTypeArg
    = TcActualType TcRefType
    | TcActualPolicy (ActorPolicy TcRefType)
    | TcActualActor (TypedActorIdSpec TcRefType)
    | TcActualLockState [TcLock TcRefType]
  deriving (Eq, Ord, Show)

------------------------------------
-- Constructors

booleanT, byteT, shortT, intT, longT,
 charT, floatT, doubleT, {-actorT,-} policyT :: TcType
booleanT = TcPrimT (BooleanT defaultPos)
byteT    = TcPrimT (ByteT    defaultPos)
shortT   = TcPrimT (ShortT   defaultPos)
intT     = TcPrimT (IntT     defaultPos)
longT    = TcPrimT (LongT    defaultPos)
charT    = TcPrimT (CharT    defaultPos)
floatT   = TcPrimT (FloatT   defaultPos)
doubleT  = TcPrimT (DoubleT  defaultPos)
--actorT   = TcPrimT (ActorT   defaultPos)
policyT  = TcPrimT (PolicyT  defaultPos)

-- | Takes a regular type and converts it to a type with state.
-- If the type is a null reference it's result will have state
-- @(MaybeNull, Committed)@, otherwise it is @(NotNull, Committed)@.
stateType :: TcType -> TcStateType
stateType t@(TcRefT TcNullT) = TcType t (MaybeNull, Committed)
stateType t = TcType t (NotNull, Committed)

unStateType :: TcStateType -> TcType
unStateType tcst = case tcst of
                     TcInstance rt _ _ _ -> TcRefT rt
--                     TcActorIdT    _   -> actorT
                     TcPolicyPolT  _   -> policyT
                     TcLockT       _   -> booleanT
                     TcType      t _  -> t

nullFromStateType :: TcStateType -> Maybe NullType
nullFromStateType tcst = case tcst of
                     TcInstance _ _ _ nt -> Just nt
--                     TcActorIdT    _   -> Nothing
                     TcPolicyPolT  _   -> Nothing
                     TcLockT       _   -> Nothing
                     TcType      _ nt  -> Just nt

nullableFromStateType :: TcStateType -> Bool
nullableFromStateType tcst = case nullFromStateType tcst of
                               (Just (MaybeNull, _)) -> True
                               _ -> False

setNullInStateType :: TcStateType -> NullType -> TcStateType
setNullInStateType tcst nt = case tcst of
                               TcInstance rt aid as _ -> TcInstance rt aid as nt
                               TcType        t _  -> TcType t nt
                               st                 -> st

nullT, voidT :: TcType
nullT    = TcRefT TcNullT
voidT    = TcVoidT

--actorIdT :: TypedActorIdSpec -> TcStateType
--actorIdT = TcActorIdT

policyPolT :: ActorPolicyBounds TcRefType -> TcStateType
policyPolT = TcPolicyPolT

lockT :: [TcLock TcRefType] -> TcStateType
lockT = TcLockT

instanceT :: TcRefType -> TypedActorIdSpec TcRefType -> [TypedActorIdSpec TcRefType] -> NullType -> TcStateType
instanceT = TcInstance

{-
clsTypeWArg :: Name () -> [TcTypeArg] -> TcType
clsTypeWArg n = TcClassT n
-}

clsType :: Ident Pa -> TcClassType
clsType = qualClsType . return

qualClsType :: [Ident Pa] -> TcClassType
qualClsType is = TcClassT (mkName_ TName PName is) []

--nameToClsType :: Name () -> TcClassType
--nameToClsType (Name _ is) = TcClassT $ map (\i -> (i,[])) is
--nameToClsType _ = panic (typesModule ++ ".nameToClsType")
--                  "AntiQName should never appear in an AST being type-checked"

stringT, objectT :: TcClassType
stringT = qualClsType $ map (Ident defaultPos . B.pack)
  ["java","lang","String"]
objectT = qualClsType $ map (Ident defaultPos . B.pack)
  ["java","lang","Object"]

nullExnT :: TcType
nullExnT = TcRefT $ TcClsRefT (qualClsType $
  map (Ident defaultPos . B.pack)
  ["java","lang","NullPointerException"])

-- promoting

clsTypeToType :: TcClassType -> TcType
clsTypeToType = TcRefT . TcClsRefT

arrayType :: TcType -> ActorPolicy TcRefType -> TcType
arrayType = (TcRefT .) . TcArrayT

mkArrayType :: TcType -> [ActorPolicy TcRefType] -> TcType
mkArrayType = foldr (flip arrayType)


-----------------------------------
-- Destructors

-- Invariant: First argument is a class type
typeName :: TcType -> Maybe (Name Pa)
typeName (TcRefT (TcClsRefT (TcClassT n tas))) =
--         let (is, args) = unzip pn in
         if null tas then Just (mkUniformName_ AmbName $ flattenName n) else Nothing
typeName _ = Nothing

typeName_ :: TcType -> Name Pa
typeName_ (TcRefT (TcClsRefT (TcClassT n _tas))) = mkUniformName_ AmbName $ flattenName n
--    let (is, _) = unzip pn in mkUniformName_ AmbName is
typeName_ t = error $ "typeName_: Not a class type: " ++ show t

--typeName_ typ = case typeName typ of
--                  Just n -> n
--                  Nothing -> error $ "typeName_: " ++ show typ

isClassType, isRefType, isPrimType, isNullType, maybeNull :: TcStateType -> Bool
isClassType (TcType (TcRefT (TcClsRefT TcClassT{})) _) = True
isClassType TcInstance{} = True
isClassType _ = False

isRefType (TcType (TcRefT _) _) = True
isRefType TcInstance{} = True
isRefType _ = False

mRefType :: TcStateType -> Maybe TcRefType
mRefType (TcType (TcRefT rTy) _) = Just rTy
mRefType (TcInstance rTy _ _ _) = Just rTy
mRefType _ = Nothing

isPrimType (TcType (TcPrimT _) _) = True
isPrimType _ = False

mNameRefType :: TcRefType -> Maybe (Name Pa)
mNameRefType (TcClsRefT (TcClassT n as)) =
    if null as then Just (mkUniformName_ AmbName $ flattenName n) else Nothing
mNameRefType _ = Nothing

isNullType (TcType (TcRefT TcNullT) _) = True
isNullType _ = False

maybeNull (TcType _ nt) = nullable nt
maybeNull (TcInstance _ _ _ nt) = nullable nt
maybeNull _ = False

mActorId :: TcStateType -> Maybe (TypedActorIdSpec TcRefType)
mActorId (TcInstance _ aid _ _) = Just aid
--mActorId (TcActorIdT aid) = Just aid
mActorId _ = Nothing

mLocks :: TcStateType -> Maybe [TcLock TcRefType]
mLocks (TcLockT ls) = Just ls
mLocks _ = Nothing

mPolicyPol :: TcStateType -> Maybe (ActorPolicyBounds TcRefType)
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

mArrayType :: TcType -> Maybe (TcType, [ActorPolicy TcRefType])
mArrayType (TcRefT (TcArrayT ty p)) = Just $
    case mArrayType ty of
      Nothing -> (ty, [p])
      Just (t, ps) -> (t, p:ps)
mArrayType _ = Nothing

mClassType :: TcType -> Maybe TcClassType
mClassType (TcRefT (TcClsRefT ct@TcClassT{})) = Just ct
mClassType _ = Nothing

mInstanceType :: TcStateType -> Maybe (TcRefType, TypedActorIdSpec TcRefType, [TypedActorIdSpec TcRefType], NullType)
mInstanceType (TcInstance rt aid aids nt) = Just (rt, aid, aids, nt)
mInstanceType _ = Nothing

-------------------------------------------
-- Type operations

widenConvert :: PrimType Pa -> [PrimType Pa]
widenConvert pt = case pt of
   FloatT  pos -> map ($ pos) [DoubleT]
   LongT   pos -> map ($ pos) [DoubleT, FloatT]
   IntT    pos -> map ($ pos) [DoubleT, FloatT, LongT]
   ShortT  pos -> map ($ pos) [DoubleT, FloatT, LongT, IntT]
   CharT   pos -> map ($ pos) [DoubleT, FloatT, LongT, IntT]
   ByteT   pos -> map ($ pos) [DoubleT, FloatT, LongT, IntT, ShortT]
   _           -> []

narrowConvert :: PrimType Pa -> [PrimType Pa]
narrowConvert pt = case pt of
   DoubleT pos -> map ($ pos) [ByteT, ShortT, CharT, IntT, LongT, FloatT]
   FloatT  pos -> map ($ pos) [ByteT, ShortT, CharT, IntT, LongT]
   LongT   pos -> map ($ pos) [ByteT, ShortT, CharT, IntT]
   IntT    pos -> map ($ pos) [ByteT, ShortT, CharT]
   CharT   pos -> map ($ pos) [ByteT, ShortT]
   ShortT  pos -> map ($ pos) [ByteT, CharT]
   _           -> []

widenNarrowConvert :: PrimType Pa -> [PrimType Pa]
widenNarrowConvert (ByteT pos) = [CharT pos]
widenNarrowConvert _           = []


box :: PrimType Pa -> Maybe TcClassType
box pt = let mkClassType str spos =
                 Just $ TcClassT
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

unbox :: TcClassType -> Maybe (PrimType Pa)
unbox (TcClassT n@(Name spos _ _ _) _) =
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


unboxType :: TcStateType -> Maybe (PrimType Pa)
unboxType sty | TcRefT (TcClsRefT ct) <- unStateType sty = unbox ct
unboxType _ = Nothing

{-
unIdent :: Ident a -> String
unIdent (Ident _ x) = B.unpack x
unIdent (AntiQIdent _ str) = panic (typesModule ++ ".unIdent")
  $ "AntiQIdent should not appear in AST being typechecked: " ++ str
-}

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


unaryNumPromote :: TcStateType -> Maybe (PrimType Pa)
unaryNumPromote sty
    | TcPrimT pt <- unStateType sty  = numPromote pt
    | Just    pt <- unboxType   sty  = numPromote pt
    | otherwise = Nothing

    where numPromote :: PrimType Pa -> Maybe (PrimType Pa)
          numPromote pt
              | pt `elem` map ($ aOfPrimType pt) [LongT, FloatT, DoubleT] = Just pt
              | pt `elem` map ($ aOfPrimType pt) [ByteT, ShortT, IntT, CharT] = Just $ IntT (aOfPrimType pt)
              | otherwise = Nothing

unaryNumPromote_ :: TcStateType -> TcStateType
unaryNumPromote_ = stateType . TcPrimT . fromJust . unaryNumPromote

binaryNumPromote :: TcStateType -> TcStateType -> Maybe (PrimType Pa)
binaryNumPromote t1 t2 = do
    pt1 <- unaryNumPromote t1
    pt2 <- unaryNumPromote t2
    return $ max pt1 pt2

binaryNumPromote_ :: TcStateType -> TcStateType -> TcStateType
binaryNumPromote_ t1 t2 = stateType . TcPrimT . fromJust $ binaryNumPromote t1 t2



---------------------------------------------
-- Pretty printing

instance Pretty TcStateType where
  pretty tcst =
      case tcst of
        --TcActorIdT aid -> text "actor[" <> pretty aid <> text "]"
        TcPolicyPolT p -> text "policy[" <> pretty p <> text "]"
        TcLockT ls -> hsep (text "lock[" : punctuate (text ",") (map pretty ls)) <> text "]"
        TcInstance ct aid aids _ -> pretty ct <> char '@' <> pretty aid <>
                                 hsep (char '{' : punctuate (char ',') (map pretty aids)) <> char '}' --TODOY prettyprint nulltype
        TcType ty nt -> pretty ty <> pretty nt

instance Pretty TcType where
  pretty tct =
      case tct of
        TcPrimT pt -> pretty pt
        TcRefT rt -> pretty rt
        TcVoidT -> text "void"

instance Pretty TcRefType where
  pretty tcrt =
      case tcrt of
        TcClsRefT ct -> pretty ct
        TcArrayT {} -> let (bt, suff) = ppArrayType (TcRefT tcrt) in bt <> suff
        TcTypeVar i -> pretty i
        TcNullT -> text "<null>"

ppArrayType :: TcType -> (Doc, Doc)
ppArrayType (TcRefT (TcArrayT ty pol)) =
    let (bt, suff) = ppArrayType ty
    in (bt, text "[]" <> char '<' <> pretty pol <> char '>' <> suff)
ppArrayType ty = (pretty ty, empty)


instance Pretty TcClassType where
  pretty (TcClassT n tas) =
      pretty n <> ppTypeParams tas
--      hcat . punctuate (char '.') $ map (\(i,tas) -> pretty i <> ppTypeParams tas) iargs

instance Pretty TcTypeArg where
  pretty (TcActualType t) = pretty t
  pretty (TcActualPolicy p) = pretty p
  pretty (TcActualActor aid) = pretty aid
  pretty (TcActualLockState ls) = ppArgs ls


ppTypeParams :: Pretty a => [a] -> Doc
ppTypeParams [] = empty
ppTypeParams tps = char '<'
    <> hsep (punctuate comma (map pretty tps))
    <> char '>'

ppArgs :: Pretty a => [a] -> Doc
ppArgs = parens . hsep . punctuate comma . map pretty

