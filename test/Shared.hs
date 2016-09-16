{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Shared where

import Data.Binary
import GHC.Generics


type EntityID = Int

data Message = EntityUpdate !EntityID !(Float, Float)
    deriving (Generic, Binary, Show)

