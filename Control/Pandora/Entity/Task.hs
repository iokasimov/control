module Control.Pandora.Entity.Task where

import "base" Data.Int (Int)
import "base" Data.String (String)
import "base" Text.Show (Show)
import "pandora" Pandora.Paradigm ((:*:) ((:*:)))

import Control.Pandora.Entity.ID (ID)
import Control.Pandora.Entity.Objective (Objective)

type Task = ID () :*: Status :*: Int :*: ID Objective :*: String :*: String :*: String

data Status = TODO | DONE | GONE | LATE deriving Show

