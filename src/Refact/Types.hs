{-# LANGUAGE DeriveFunctor #-}
module Refact.Types where

-- Almost all from the Annotated module, but the fixity resolution from Annotated
-- uses the unannotated Assoc enumeration, so export that instead

data SrcSpan = SrcSpan
                { start :: (Int, Int)
                , end :: (Int, Int) } deriving (Read, Show, Eq, Ord)

data RType = Expr | Decl | Type | Pattern deriving (Read, Ord, Show, Eq)

data Refactoring a =
  Replace  {
      rtype :: RType -- ^ Type of expression to be replaced
    , expr :: a  -- ^ Expression to replace
    , subts :: [(String, a)] -- ^ Substitutions to make
    , orig  :: String -- ^ Replacment template
    }
  | ModifyComment {
      originalComment :: String
    , newComment :: String
    } deriving (Show, Read, Functor, Eq, Ord)



