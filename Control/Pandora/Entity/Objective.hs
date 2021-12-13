module Control.Pandora.Entity.Objective where

import "base" Data.Int (Int)
import "base" Data.String (String)
import "pandora" Pandora.Paradigm ((:*:) ((:*:)))

import Control.Pandora.Entity.ID (ID)

type Objective = ID () :*: String
