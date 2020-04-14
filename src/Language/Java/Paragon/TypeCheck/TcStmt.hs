{-# LANGUAGE TupleSections, BangPatterns #-}
module Language.Java.Paragon.TypeCheck.TcStmt where

import Language.Java.Paragon.Syntax
import Language.Java.Paragon.Pretty
import Language.Java.Paragon.Interaction
import Language.Java.Paragon.Error
import Language.Java.Paragon.SourcePos

import qualified Language.Java.Paragon.PolicyLang as PL
import Language.Java.Paragon.TypeCheck.Monad
import Language.Java.Paragon.TypeCheck.Types
import Language.Java.Paragon.TypeCheck.TypeMap
import Language.Java.Paragon.Decorations.PaDecoration


import Language.Java.Paragon.TypeCheck.TcExp

import Control.Monad (unless, void)
import Control.Applicative ((<$>))

import qualified Data.ByteString.Char8 as B

----------------------------------------
-- Typechecking Blocks and BlockStmts --
----------------------------------------

tcBlock :: TypeCheck (CodeM TC) Block
tcBlock (Block _ bss) = TcBlock <$> insideBlock (tcBlockStmts bss)

insideBlock :: CodeM TC a -> CodeM TC a
insideBlock = withEnv (\env -> return $ env { tcVars = emptyVarMap : tcVars env })

tcBlockStmts :: [BlockStmt PA] -> CodeM TC [BlockStmt TC]
-- Rule EMPTYBLOCK
tcBlockStmts [] = return []

-- Rule BLOCKSTMT
tcBlockStmts (BlockStmt _ stmt : bss) = do -- TODO: Maybe this can be a fold?
  stmt' <- tcStmt stmt
  bss'  <- tcBlockStmts bss
  return (TcBlockStmt stmt':bss')

-- Rule LOCALVARINIT/LOCALVARDECL
tcBlockStmts (LocalVars _ ms t vds : bss) = error "tcBlockStmts: case not implemented"

tcBlockStmts (b:_bss) = fail $ "Unsupported block statement: " ++ prettyPrint b


-----------------------------------
--    Typechecking Statements    --
-----------------------------------

tcStmt :: TypeCheck (CodeM TC) Stmt
-- Rule EMPTY
tcStmt (Empty _) = return $ TcEmpty

-- Rule EXPSTMT
tcStmt (ExpStmt _ e) = do
  (_, e') <- tcExp e
  return $ TcExpStmt e'

-- Rule BLOCK
tcStmt (StmtBlock _ b) = TcStmtBlock <$> tcBlock b

tcStmt a@IfThen{} = error "tcStmt: case IfThen not implemented"
tcStmt a@IfThenElse{} = error "tcStmt: case IfThenElse not implemented"
tcStmt a@While{} = error "tcStmt: case While not implemented"
tcStmt a@BasicFor{} = error "tcStmt: case BasicFor not implemented"
tcStmt a@EnhancedFor{} = error "tcStmt: case EnhancedFor not implemented"
tcStmt a@Assert{} = error "tcStmt: case Assert not implemented"
tcStmt a@Switch{} = error "tcStmt: case Switch not implemented"
tcStmt a@Do{} = error "tcStmt: case Do not implemented"
tcStmt a@Break{} = error "tcStmt: case Break not implemented"
tcStmt a@Continue{} = error "tcStmt: case Continue not implemented"
tcStmt a@Synchronized{} = error "tcStmt: case Synchronized not implemented"
tcStmt a@Throw{} = error "tcStmt: case Throw not implemented"
tcStmt a@Try{} = error "tcStmt: case Try not implemented"
tcStmt a@Labeled{} = error "tcStmt: case Labeled not implemented"
tcStmt a@Open{} = error "tcStmt: case Open not implemented"
tcStmt a@Close{} = error "tcStmt: case Close not implemented"
tcStmt a@OpenBlock{} = error "tcStmt: case OpenBlock not implemented"
tcStmt a@CloseBlock{} = error "tcStmt: case CloseBlock not implemented"