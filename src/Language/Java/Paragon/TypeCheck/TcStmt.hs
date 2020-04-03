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

tcBlock :: TypeCheck TcCodeM Block
tcBlock (Block _ bss) = TcBlock <$> insideBlock (tcBlockStmts bss)

insideBlock :: TcCodeM a -> TcCodeM a
insideBlock = withEnv (\env -> return $ env { vars = emptyVarMap : vars env })

tcBlockStmts :: [BlockStmt PA] -> TcCodeM [BlockStmt TC]
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

tcStmt :: TypeCheck TcCodeM Stmt
-- Rule EMPTY
tcStmt (Empty _) = return $ TcEmpty

-- Rule EXPSTMT
tcStmt (ExpStmt _ e) = do
  (_, e') <- tcExp e
  return $ TcExpStmt e'

-- Rule BLOCK
tcStmt (StmtBlock _ b) = TcStmtBlock <$> tcBlock b