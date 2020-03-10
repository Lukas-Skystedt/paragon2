{-# LANGUAGE TemplateHaskell #-}
module Language.Java.Paragon.Decorations.DecorationTypes where

import Data.Void
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Control.Monad (join)

-- | Data type for when an extension field is not used. (As in Trees That Grow)
type NoFieldExt = ()
-- | Data type for when a extension constructor is not used. (As in Trees That Grow)
type NoConExt = Void


-- -| TODO: It would be nice to have something like this. However, \$family does
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


makeTypeInsts ::  Name -> Name -> [Name] -> DecsQ
makeTypeInsts ind typ fams = join <$> mapM (makeTypeInst ind typ) fams
