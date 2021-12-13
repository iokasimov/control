module Control.Pandora.Entity.Task where

import "pandora" Pandora.Paradigm ((:*:) ((:*:)))

import Control.Pandora.Entity.ID (ID)
import Control.Pandora.Entity.Objective (Objective)

type Task = ID () :*: Status :*: Int :*: ID Objective :*: String :*: String :*: String

instance Show Task where
	show (_ :*: status :*: mode :*: oid :*: title :*: start :*: stop) =
		"[" <> show status <> "] " <> "{" <> start <> " - " <> stop <> "} " <> title

data Status = TODO | DONE | GONE | LATE deriving Show
