module Control.Pandora.Entity.ID where

import "base" Data.Int (Int)

newtype ID (e :: k) = ID { unid :: Int }
