-- | Types and functions related to AST decorations.
--
-- Decoration-related types and functions that are not specific to any phase
-- lives in this module. Notably, it contains template Haskell functions for
-- deriving type instances and pattern synonyms for the AST.
module Language.Java.Paragon.Decorations.DecorationTypes where

import Data.Void
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Control.Monad (join)

-- | Data type for when an extension field is not used. (As in Trees That Grow)
type NoFieldExt = ()

-- | Data type for when a extension constructor is not used. (As in Trees That Grow)
type NoConExt = Void


-- | Create type instances for each of the families given, all with the same
-- index and result type. See 'makeTypeInst'.
makeTypeInsts ::  Name -> Name -> [Name] -> DecsQ
makeTypeInsts ind typ fams = join <$> mapM (makeTypeInst ind typ) fams


-- | Given a type index, a type and a type family, creates a corresponding type
-- instance on the form
-- > type instance fam ind = typ
--
-- TODO: It would be nice to have something like this. However, \$family does
-- not work.
-- > [d| type instance $family $i = $t |]
-- > where
-- >   i = conT index
-- >   t = conT typ
makeTypeInst :: Name -> Name -> Name -> DecsQ
makeTypeInst ind typ fam = return [ TySynInstD fam $
                                    TySynEqn
                                    [ConT ind]
                                    (ConT typ)
                                  ]


-- | Split a qualified name to an unqualified name and the prefix. Eg.
-- "GHC.Maybe.Just" should return ("GHC.Maybe.Just", "Just").
takeUnqualified :: String -> (String, String)
takeUnqualified name = let (suffR, preR) = break (=='.') $ reverse name
                       in (reverse preR, reverse suffR)


-- | Simply run 'makePatternSyn' for every data constructor in the list.
makePatternSyns :: String -> [Name] -> Q Pat -> DecsQ
makePatternSyns prefix conNames rhPatQ = join <$>
  mapM (\conName -> makePatternSyn prefix conName rhPatQ) conNames


-- | Creates a pattern synonym for a given constructor (by 'Name'). It is
-- intended to be used for creating patterns for the extension field in data
-- constructors under the TTG paradigm.
--
-- The new pattern will be named the same as the given one, but prefixed with
-- the provided 'String'.
--
-- The provided pattern will be used for the /first/ argument of the constructor
-- (the extension field). Every name bound in this pattern will be prepended to
-- the LHS pattern.
--
-- For example,
-- > $(makePatternSyn "MyPre" 'SwitchBlock [p| (a, (b, c)) |])
-- will generate the pattern (with somewhat other names)
-- > pattern MyPreSwitchBlock a b c d e = SwitchBlock (a, (b, c)) d e
makePatternSyn :: String -> Name -> Q Pat -> DecsQ
makePatternSyn prefix conName rhPatQ = do
          rhPat <- rhPatQ

          let (_pre, suff) = takeUnqualified $ show conName
          let newConName = mkName $ prefix ++ suff

          -- The type given here is annoying to work with since it consists of
          -- applications..
          (DataConI _nam _typ parNam) <- reify conName
          let (VarP temp) = rhPat
          -- ..Instead we extract its parent (the type it constructs),..
          (DataConI name typ par) <- reify conName
          -- ..get its declaration..
          (TyConI dec) <- reify par
          -- ..and find the constructor again,..
          let [NormalC _ bangTypes] = case dec of
                (DataD    _ctx _name _binds _kind cons _deriv) -> filter (\(NormalC n _) -> n == conName) cons
                (NewtypeD _ctx _name _binds _kind con  _deriv) -> [con]
          -- now with the type given as a list. We throw away the first field,
          -- which should be the TTG extension field.
          let (_extfield:conArgs) = map snd bangTypes :: [Type]

          -- Extract all the names that are used in a pattern in the right hand
          -- side.
          let patNames = patternNames rhPat
          -- Generate names for the remaining constructor fields.
          bindingNames <- mapM (const (newName "a")) conArgs

          let lhsPattern = PrefixPatSyn $ patNames ++ bindingNames
          let rhsPattern = ConP conName $ rhPat : map VarP bindingNames

          return [ PatSynD newConName lhsPattern ImplBidir rhsPattern ]


-- | Given a pattern, find all 'VarP' recursively and extract their names. That
-- is, find all variable bindings in a pattern.
patternNames :: Pat -> [Name]
patternNames (LitP _) = []
patternNames (VarP name) = [name]
patternNames (TupP pats) = concatMap patternNames pats
patternNames (UnboxedTupP pats) = concatMap patternNames pats
patternNames (UnboxedSumP pat _ _) = patternNames pat
patternNames (ConP _ pats) = concatMap patternNames pats
patternNames (InfixP pat1 _ pat2) = patternNames pat1 ++ patternNames pat2
patternNames (UInfixP pat1 _ pat2) = patternNames pat1 ++ patternNames pat2
patternNames (ParensP pat) = patternNames pat
patternNames (TildeP pat) = patternNames pat
patternNames (BangP pat) = patternNames pat
patternNames (AsP _ pat) = patternNames pat
patternNames WildP = []
patternNames (RecP _ fpats) = concatMap (patternNames . snd) fpats
patternNames (ListP pats) = concatMap patternNames pats
patternNames (SigP pat _) = patternNames pat
patternNames (ViewP _ pat) = patternNames pat
