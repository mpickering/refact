{-# LANGUAGE DeriveFunctor #-}
module Refact.Types where

-- Almost all from the Annotated module, but the fixity resolution from Annotated
-- uses the unannotated Assoc enumeration, so export that instead

data SrcSpan = SrcSpan
                { start :: (Int, Int)
                , end :: (Int, Int) } deriving (Read, Show)

data Refactoring a =
  Replace  {
      expr :: a  -- ^ Expression to replace
    , subts :: [(String, a)] -- ^ Substitutions to make
    , orig  :: String -- ^ Replacment template
    } deriving (Show, Read, Functor)



