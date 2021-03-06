module Control.Entity.Objective where

import "base" Data.Int (Int)
import "base" Data.String (String)
import "pandora" Pandora.Paradigm ((:*:) ((:*:)))

import Control.Entity.ID (ID)

type Objective = ID () :*: String

instance Show Objective where
	show (_ :*: title) = title
