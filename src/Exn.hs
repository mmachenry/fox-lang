module Exn (
    FoxExn(..),
    ) where

data FoxExn =
      DynamicError String
    | StaticError String
    | TypeError String
    -- | UserError Value
    deriving (Eq, Show)

