module GaErr where

data GaErr
  = DbErr String
  | AuthErr String
  | FSErr String
  | UnexpectedErr String
 deriving (Show)
